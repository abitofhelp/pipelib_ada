--   =============================================================================
--   Pipelib.Infrastructure.IO.Random_Write_File - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;

package body Pipelib.Infrastructure.Adapters.IO.Random_Write_File is

   use Ada.Streams.Stream_IO;
   use Abohlib.Core.Domain.Value_Objects.File_Path;
   use Pipelib.Core.Domain.Value_Objects.File_Chunk;

   --  Local constants (literal matching abohlib SI value)
   SI_MB : constant := 1_000_000;
   pragma Unreferenced (SI_MB);

   -- -----------
   --  Create
   -- -----------

   function Create
     (Path          : Abohlib.Core.Domain.Value_Objects.File_Path.File_Path;
      Expected_Size : Long_Long_Integer := 0;
      Use_Temp_File : Boolean := True) return Random_Write_File_Access
   is
      File      : constant Random_Write_File_Access := new Random_Write_File;
      Temp_Name : Unbounded_String;
   begin
      File.File_Path := Path;
      File.Use_Temp := Use_Temp_File;

      if Use_Temp_File then
         --  Create temp file name
         Temp_Name := To_Unbounded_String (To_String (Path) & ".tmp");
         File.Temp_Path := Create (To_String (Temp_Name), Output);

         --  Create/open temp file
         null;  -- Creating temp file
         Create (File.File_Handle, Out_File, To_String (File.Temp_Path));
      else
         --  Create/open target file directly
         null;  -- Creating output file
         Create (File.File_Handle, Out_File, To_String (Path));
      end if;

      File.Is_Open_Flag := True;

      --  Pre-allocate space if size is known
      if Expected_Size > 0 then
         File.Preallocate (Expected_Size);
      end if;

      return File;
   exception
      when others =>
         if Is_Open (File.File_Handle) then
            Close (File.File_Handle);
         end if;
         raise;
   end Create;

   -- -----------------------------
   --  Write_Chunk_At_Position
   -- -----------------------------

   overriding
   procedure Write_Chunk_At_Position
     (Writer   : in out Random_Write_File;
      Chunk    : File_Chunk_Type;
      Position : Long_Long_Integer)
   is
      File : Random_Write_File renames Writer;
   begin
      --  Seek to the specified position
      Set_Index (File.File_Handle, Positive_Count (Position + 1));

      --  Write chunk data
      declare
         S          : constant Stream_Access := Stream (File.File_Handle);
         Chunk_Data : constant Stream_Element_Array := Data (Chunk);
      begin
         Stream_Element_Array'Write (S, Chunk_Data);
      end;

      --  Update chunk size if not set
      if File.Chunk_Size = 0 then
         File.Chunk_Size := Data_Length (Chunk);
      end if;
   end Write_Chunk_At_Position;

   -- ---------------------
   --  Write_Chunk_At
   -- ---------------------

   procedure Write_Chunk_At
     (Writer   : in out Random_Write_File;
      Chunk    : File_Chunk_Type;
      Position : Long_Long_Integer;
      Result   : in out Write_Result.Result)
   is
      use Write_Result;
      File : Random_Write_File renames Writer;
   begin
      begin
         Write_Chunk_At_Position (File, Chunk, Position);
         Result := Ok (True);
      exception
         when E : others =>
            Result :=
              Err
                (To_Unbounded_String
                   ("Failed to write chunk: "
                    & Ada.Exceptions.Exception_Message (E)));
      end;
   end Write_Chunk_At;

   -- -----------------
   --  Write_Chunk
   -- -----------------

   overriding
   procedure Write_Chunk
     (Writer : in out Random_Write_File; Chunk : File_Chunk_Type)
   is
      File     : Random_Write_File renames Writer;
      Position : constant Long_Long_Integer :=
        Long_Long_Integer (Sequence_Number (Chunk))
        * Long_Long_Integer (Data_Length (Chunk));
   begin
      Write_Chunk_At_Position (File, Chunk, Position);
   end Write_Chunk;

   -- ------------
   --  Is_Open
   -- ------------

   overriding
   function Is_Open (Writer : Random_Write_File) return Boolean is
      File : Random_Write_File renames Writer;
   begin
      return File.Is_Open_Flag and then Is_Open (File.File_Handle);
   end Is_Open;

   -- --------
   --  Size
   -- --------

   function Size (File : Random_Write_File) return Long_Long_Integer is
   begin
      return Long_Long_Integer (Ada.Streams.Stream_IO.Size (File.File_Handle));
   end Size;

   -- ----------
   --  Commit
   -- ----------

   overriding
   function Commit
     (Writer : in out Random_Write_File)
      return Pipelib
               .Core
               .Domain
               .Ports
               .File_Writer_Interface
               .Write_Result
               .Result
   is
      File : Random_Write_File renames Writer;
      use Pipelib.Core.Domain.Ports.File_Writer_Interface.Write_Result;
   begin
      if not File.Is_Open_Flag then
         return Err (To_Unbounded_String ("File is not open"));
      end if;

      --  Flush buffers
      Flush (File);

      --  Close the file
      Close (File.File_Handle);
      File.Is_Open_Flag := False;

      --  If using temp file, rename to final
      if File.Use_Temp then
         begin
            --  Delete target if it exists
            if Ada.Directories.Exists (To_String (File.File_Path)) then
               Ada.Directories.Delete_File (To_String (File.File_Path));
            end if;

            --  Rename temp to target
            Ada.Directories.Rename
              (Old_Name => To_String (File.Temp_Path),
               New_Name => To_String (File.File_Path));
         exception
            when E : others =>
               return
                 Err
                   (To_Unbounded_String
                      ("Failed to rename temp file: "
                       & Ada.Exceptions.Exception_Message (E)));
         end;
      end if;

      return Ok (True);
   end Commit;

   -- ------------
   --  Rollback
   -- ------------

   procedure Rollback (File : in out Random_Write_File) is
   begin
      if File.Is_Open_Flag then
         Close (File.File_Handle);
         File.Is_Open_Flag := False;
      end if;

      --  Delete temp file if it exists
      if File.Use_Temp
        and then Ada.Directories.Exists (To_String (File.Temp_Path))
      then
         Ada.Directories.Delete_File (To_String (File.Temp_Path));
      end if;
   end Rollback;

   -- ---------
   --  Close
   -- ---------

   overriding
   procedure Close (Writer : in out Random_Write_File) is
      File : Random_Write_File renames Writer;
   begin
      if File.Is_Open_Flag then
         if Is_Open (File.File_Handle) then
            Close (File.File_Handle);
         end if;
         File.Is_Open_Flag := False;
      end if;

      --  If using temp file, rename to final
      if File.Use_Temp then
         begin
            --  Delete target if it exists
            if Ada.Directories.Exists (To_String (File.File_Path)) then
               Ada.Directories.Delete_File (To_String (File.File_Path));
            end if;

            --  Rename temp to target
            Ada.Directories.Rename
              (Old_Name => To_String (File.Temp_Path),
               New_Name => To_String (File.File_Path));
         exception
            when others =>
               null;  -- Ignore rename errors in Close
         end;
      end if;
   end Close;

   -- ---------
   --  Flush
   -- ---------

   procedure Flush (File : in out Random_Write_File) is
   begin
      Ada.Streams.Stream_IO.Flush (File.File_Handle);
   end Flush;

   -- ---------------
   --  Preallocate
   -- ---------------

   procedure Preallocate
     (File : in out Random_Write_File; Size : Long_Long_Integer) is
   begin
      --  Platform-specific implementation
      --  On systems without fallocate, we just seek to the end and write a byte
      Set_Index (File.File_Handle, Positive_Count (Size));
      declare
         Byte : constant Stream_Element_Array (1 .. 1) := [others => 0];
      begin
         declare
            S : constant Stream_Access := Stream (File.File_Handle);
         begin
            Stream_Element_Array'Write (S, Byte);
         end;
      end;

      --  Seek back to beginning
      Set_Index (File.File_Handle, 1);
   end Preallocate;

   -- -----------
   --  Destroy
   -- -----------

   procedure Destroy (File : in out Random_Write_File_Access) is
      procedure Free is new
        Ada.Unchecked_Deallocation
          (Random_Write_File,
           Random_Write_File_Access);
   begin
      if File /= null then
         File.Close;
         declare
            F : Random_Write_File_Access := File;
         begin
            Free (F);
            File := null;
         end;
      end if;
   end Destroy;

   -- ------------
   --  Finalize
   -- ------------

   overriding
   procedure Finalize (File : in out Random_Write_File) is
   begin
      Rollback (File);  -- Clean up on finalization
   end Finalize;

   -- --------------------------------
   --  Protected_Random_Write_File
   -- --------------------------------

   protected body Protected_Random_Write_File is

      -- ---------------
      --  Initialize
      -- ---------------

      procedure Initialize (File : Random_Write_File_Access) is
      begin
         File_Access := File;
      end Initialize;

      -- ----------------
      --  Write_Chunk
      -- ----------------

      procedure Write_Chunk (Chunk : File_Chunk_Type) is
      begin
         if File_Access /= null then
            File_Access.Write_Chunk (Chunk);
         end if;
      end Write_Chunk;

      -- ----------------------
      --  Write_At_Position
      -- ----------------------

      procedure Write_At_Position
        (Chunk : File_Chunk_Type; Position : Long_Long_Integer) is
      begin
         if File_Access /= null then
            File_Access.Write_Chunk_At_Position (Chunk, Position);
         end if;
      end Write_At_Position;

      -- -------------
      --  Get_Size
      -- -------------

      function Get_Size return Long_Long_Integer is
      begin
         if File_Access /= null and then File_Access.Is_Open then
            return File_Access.Size;
         else
            return 0;
         end if;
      end Get_Size;

      -- ------------
      --  Is_Ready
      -- ------------

      function Is_Ready return Boolean is
      begin
         return File_Access /= null and then File_Access.Is_Open;
      end Is_Ready;

   end Protected_Random_Write_File;

end Pipelib.Infrastructure.Adapters.IO.Random_Write_File;
