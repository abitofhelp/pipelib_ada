--   =============================================================================
--   Pipelib.Core.Application.Services.Parallel_Chunk_Processor - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Containers;

package body Pipelib.Core.Application.Services.Parallel_Chunk_Processor is

   use Pipelib.Infrastructure.IO.Random_Write_File;

   -- ----------
   --  Create
   -- ----------

   function Create
     (Worker_Count : Positive;
      Output_File : Random_Write_File_Access;
      Context : Context_Type) return Parallel_Processor_Access
   is
      Processor : constant Parallel_Processor_Access := new Parallel_Processor_Type;
   begin
      Processor.Worker_Count := Worker_Count;
      Processor.Context := Context;

      --  Create protected wrapper for output file
      Processor.Output_File := new Protected_Random_Write_File;
      Processor.Output_File.Initialize (Output_File);

      --  Create worker tasks
      Processor.Workers := new Worker_Task_Array (1 .. Worker_Count);
      for I in Processor.Workers'Range loop
         Processor.Workers (I) := new Worker_Task_Type (Processor);
      end loop;

      return Processor;
   end Create;

   -- ---------
   --  Start
   -- ---------

   procedure Start (Processor : in out Parallel_Processor_Type) is
   begin
      if not Processor.Is_Running then
         Processor.Is_Running := True;
         Processor.End_Of_Input := False;

         --  Start all worker tasks
         for Worker of Processor.Workers.all loop
            Worker.Start;
         end loop;
      end if;
   end Start;

   -- -----------------
   --  Submit_Chunk
   -- -----------------

   procedure Submit_Chunk
     (Processor : in out Parallel_Processor_Type;
      Chunk : File_Chunk_Type)
   is
      Item : constant Work_Item := (Chunk => Chunk, Is_End_Marker => False);
   begin
      Processor.Work_Queue.Enqueue (Item);
   end Submit_Chunk;

   -- ------------------------
   --  Signal_End_Of_Input
   -- ------------------------

   procedure Signal_End_Of_Input (Processor : in out Parallel_Processor_Type) is
      End_Marker : Work_Item;
   begin
      Processor.End_Of_Input := True;

      --  Send end marker to each worker
      for I in 1 .. Processor.Worker_Count loop
         End_Marker.Is_End_Marker := True;
         Processor.Work_Queue.Enqueue (End_Marker);
      end loop;
   end Signal_End_Of_Input;

   -- ------------------------
   --  Wait_For_Completion
   -- ------------------------

   procedure Wait_For_Completion (Processor : in out Parallel_Processor_Type) is
   begin
      --  Wait for all workers to complete
      for Worker of Processor.Workers.all loop
         while not Worker'Terminated loop
            delay 0.01;  -- Small delay to avoid busy waiting
         end loop;
      end loop;

      Processor.Is_Running := False;
   end Wait_For_Completion;

   -- --------
   --  Stop
   -- --------

   procedure Stop (Processor : in out Parallel_Processor_Type) is
   begin
      if Processor.Is_Running then
         --  Signal workers to stop
         for Worker of Processor.Workers.all loop
            Worker.Stop;
         end loop;

         --  Clear the queue by dequeuing all items
         declare
            use Ada.Containers;
            Item : Work_Item;
         begin
            while Processor.Work_Queue.Current_Use > 0 loop
               select
                  Processor.Work_Queue.Dequeue (Item);
               or
                  delay 0.001;
                  exit;
               end select;
            end loop;
         end;

         Processor.Is_Running := False;
      end if;
   end Stop;

   -- --------------
   --  Is_Running
   -- --------------

   function Is_Running (Processor : Parallel_Processor_Type) return Boolean is
   begin
      return Processor.Is_Running;
   end Is_Running;

   -- ---------------------
   --  Chunks_Processed
   -- ---------------------

   function Chunks_Processed (Processor : Parallel_Processor_Type) return Natural is
   begin
      return Processor.Statistics.Get_Processed;
   end Chunks_Processed;

   -- --------------
   --  Get_Error
   -- --------------

   function Get_Error (Processor : Parallel_Processor_Type) return Unbounded_String is
   begin
      return Processor.Statistics.Get_Error;
   end Get_Error;

   -- --------------
   --  Has_Error
   -- --------------

   function Has_Error (Processor : Parallel_Processor_Type) return Boolean is
   begin
      return Processor.Statistics.Has_Error;
   end Has_Error;

   -- -----------
   --  Destroy
   -- -----------

   procedure Destroy (Processor : in out Parallel_Processor_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Parallel_Processor_Type, Parallel_Processor_Access);
      type Worker_Task_Array_Access is access all Worker_Task_Array;
      procedure Free_Array is new Ada.Unchecked_Deallocation
        (Worker_Task_Array, Worker_Task_Array_Access);
      procedure Free_Protected is new Ada.Unchecked_Deallocation
        (Protected_Random_Write_File, Protected_Random_Write_File_Access);
   begin
      if Processor /= null then
         --  Stop processing if running
         Processor.Stop;

         --  Clean up resources
         if Processor.Workers /= null then
            declare
               W : Worker_Task_Array_Access := Worker_Task_Array_Access (Processor.Workers);
            begin
               Free_Array (W);
               Processor.Workers := null;
            end;
         end if;

         if Processor.Output_File /= null then
            Free_Protected (Processor.Output_File);
         end if;

         declare
            P : Parallel_Processor_Access := Processor;
         begin
            Free (P);
            Processor := null;
         end;
      end if;
   end Destroy;

   -- --------------------
   --  Statistics_Type
   -- --------------------

   protected body Statistics_Type is

      procedure Increment_Processed is
      begin
         Processed_Count := Processed_Count + 1;
      end Increment_Processed;

      procedure Set_Error (Msg : Unbounded_String) is
      begin
         if Length (Error_Message) = 0 then  -- Keep first error
            Error_Message := Msg;
         end if;
      end Set_Error;

      function Get_Processed return Natural is
      begin
         return Processed_Count;
      end Get_Processed;

      function Get_Error return Unbounded_String is
      begin
         return Error_Message;
      end Get_Error;

      function Has_Error return Boolean is
      begin
         return Length (Error_Message) > 0;
      end Has_Error;

   end Statistics_Type;

   -- ---------------------
   --  Worker_Task_Type
   -- ---------------------

   task body Worker_Task_Type is
      Item : Work_Item;
      Processed_Chunk : File_Chunk_Type;
      Should_Stop : Boolean := False;
   begin
      --  Wait for start signal
      accept Start;

      --  Main processing loop
      loop
         select
            accept Stop;
            Should_Stop := True;
         else
            --  Try to get work item (non-blocking)
            select
               Parent.Work_Queue.Dequeue (Item);

               --  Check for end marker
               exit when Item.Is_End_Marker;

               --  Process the chunk
               begin
                  Processed_Chunk := Process_Chunk (Item.Chunk, Parent.Context);

                  --  Write to output file at correct position
                  Parent.Output_File.Write_Chunk (Processed_Chunk);

                  --  Update statistics
                  Parent.Statistics.Increment_Processed;

               exception
                  when E : others =>
                     Parent.Statistics.Set_Error
                       (To_Unbounded_String
                         ("Worker error: " & Ada.Exceptions.Exception_Message (E)));
                     exit;  -- Stop on error
               end;

            or
               delay 0.001;  -- Small delay if queue is empty
            end select;
         end select;

         exit when Should_Stop;
      end loop;

   exception
      when E : others =>
         Parent.Statistics.Set_Error
           (To_Unbounded_String
             ("Worker task error: " & Ada.Exceptions.Exception_Message (E)));
   end Worker_Task_Type;

end Pipelib.Core.Application.Services.Parallel_Chunk_Processor;
