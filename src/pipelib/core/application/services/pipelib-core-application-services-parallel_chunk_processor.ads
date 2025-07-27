--   =============================================================================
--   Pipelib.Core.Application.Services.Parallel_Chunk_Processor
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Processes chunks in parallel using multiple worker tasks.
--   Leverages random write capability for out-of-order chunk processing.
--   =============================================================================

pragma Ada_2022;

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Pipelib.Core.Domain.Value_Objects.File_Chunk;
with Pipelib.Infrastructure.IO.Random_Write_File;
with Abohlib.Core.Domain.Result;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
   type Context_Type is private;
   with function Process_Chunk
     (Chunk : Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type;
      Context : Context_Type)
      return Pipelib.Core.Domain.Value_Objects.File_Chunk.File_Chunk_Type;

package Pipelib.Core.Application.Services.Parallel_Chunk_Processor is

   use Pipelib.Core.Domain.Value_Objects.File_Chunk;

   --  Result types
   package Process_Result is new Abohlib.Core.Domain.Result.Result_Package
     (Ok_Type => Boolean,
      Err_Type => Unbounded_String);

   --  Parallel processor type
   type Parallel_Processor_Type is tagged limited private;
   type Parallel_Processor_Access is access all Parallel_Processor_Type;

   --  Create processor with specified worker count
   function Create
     (Worker_Count : Positive;
      Output_File : Pipelib.Infrastructure.IO.Random_Write_File.Random_Write_File_Access;
      Context : Context_Type) return Parallel_Processor_Access
     with Pre => Worker_Count <= 64,  -- Reasonable limit
          Post => Create'Result /= null;

   --  Start processing
   procedure Start (Processor : in out Parallel_Processor_Type);

   --  Submit chunk for processing
   procedure Submit_Chunk
     (Processor : in out Parallel_Processor_Type;
      Chunk : File_Chunk_Type)
     with Pre => Processor.Is_Running;

   --  Signal no more chunks will be submitted
   procedure Signal_End_Of_Input (Processor : in out Parallel_Processor_Type)
     with Pre => Processor.Is_Running;

   --  Wait for all chunks to be processed
   procedure Wait_For_Completion (Processor : in out Parallel_Processor_Type);

   --  Stop processing (emergency shutdown)
   procedure Stop (Processor : in out Parallel_Processor_Type);

   --  Query methods
   function Is_Running (Processor : Parallel_Processor_Type) return Boolean;
   function Chunks_Processed (Processor : Parallel_Processor_Type) return Natural;
   function Get_Error (Processor : Parallel_Processor_Type) return Unbounded_String;
   function Has_Error (Processor : Parallel_Processor_Type) return Boolean;

   --  Destroy processor
   procedure Destroy (Processor : in out Parallel_Processor_Access);

private

   --  Work item for processing
   type Work_Item is record
      Chunk : File_Chunk_Type;
      Is_End_Marker : Boolean := False;
   end record;

   --  Queue for work items
   package Work_Queue_Interface is new
      Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Work_Item);

   package Work_Queues is new
      Ada.Containers.Unbounded_Synchronized_Queues
         (Queue_Interfaces => Work_Queue_Interface);

   --  Statistics tracking
   protected type Statistics_Type is
      procedure Increment_Processed;
      procedure Set_Error (Msg : Unbounded_String);
      function Get_Processed return Natural;
      function Get_Error return Unbounded_String;
      function Has_Error return Boolean;
   private
      Processed_Count : Natural := 0;
      Error_Message : Unbounded_String;
   end Statistics_Type;

   --  Worker task type
   task type Worker_Task_Type (Parent : access Parallel_Processor_Type) is
      entry Start;
      entry Stop;
   end Worker_Task_Type;

   type Worker_Task_Access is access Worker_Task_Type;
   type Worker_Task_Array is array (Positive range <>) of Worker_Task_Access;

   --  Main processor type
   type Parallel_Processor_Type is tagged limited record
      Work_Queue : Work_Queues.Queue;
      Output_File : Pipelib.Infrastructure.IO.Random_Write_File.Protected_Random_Write_File_Access;
      Context : Context_Type;
      Workers : access Worker_Task_Array;
      Worker_Count : Positive;
      Statistics : Statistics_Type;
      Is_Running : Boolean := False;
      End_Of_Input : Boolean := False;
   end record;

end Pipelib.Core.Application.Services.Parallel_Chunk_Processor;
