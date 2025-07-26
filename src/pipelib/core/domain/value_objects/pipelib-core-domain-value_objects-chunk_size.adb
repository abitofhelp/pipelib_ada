--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.Chunk_Size - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

package body Pipelib.Core.Domain.Value_Objects.Chunk_Size is

   -- -------------------
   --  Create
   -- -------------------

   function Create (Bytes : Natural) return Chunk_Size_Type is
   begin
      if Bytes < MIN_SIZE then
         raise Invalid_Chunk_Size
           with
             "Chunk size"
             & Bytes'Image
             & " is below minimum of"
             & MIN_SIZE'Image
             & " bytes";
      end if;

      if Bytes > MAX_SIZE then
         raise Invalid_Chunk_Size
           with
             "Chunk size"
             & Bytes'Image
             & " exceeds maximum of"
             & MAX_SIZE'Image
             & " bytes";
      end if;

      return (Ada.Finalization.Controlled with Bytes_Value => Bytes);
   end Create;

   -- ----------------------
   --  Create_From_KB
   -- ----------------------

   function Create_From_KB (KB : Natural) return Chunk_Size_Type is
   begin
      return Create (KB * SI_KB);
   end Create_From_KB;

   -- ----------------------
   --  Create_From_MB
   -- ----------------------

   function Create_From_MB (MB : Natural) return Chunk_Size_Type is
   begin
      return Create (MB * SI_MB);
   end Create_From_MB;

   -- -------------------
   --  Create_Default
   -- -------------------

   function Create_Default return Chunk_Size_Type is
   begin
      return (Ada.Finalization.Controlled with Bytes_Value => DEFAULT_SIZE);
   end Create_Default;

   -- ---------
   --  Bytes
   -- ---------

   function Bytes (Size : Chunk_Size_Type) return Natural is
   begin
      return Size.Bytes_Value;
   end Bytes;

   -- ------------
   --  As_Bytes
   -- ------------

   function As_Bytes (Size : Chunk_Size_Type) return Natural is
   begin
      return Size.Bytes_Value;
   end As_Bytes;

   -- -------------
   --  Kilobytes
   -- -------------

   function Kilobytes (Size : Chunk_Size_Type) return Float is
   begin
      return Float (Size.Bytes_Value) / Float (SI_KB);
   end Kilobytes;

   -- -------------
   --  Megabytes
   -- -------------

   function Megabytes (Size : Chunk_Size_Type) return Float is
   begin
      return Float (Size.Bytes_Value) / Float (SI_MB);
   end Megabytes;

   -- -------------------------
   --  Optimal_For_File_Size
   -- -------------------------

   function Optimal_For_File_Size
     (File_Size : Long_Long_Integer) return Chunk_Size_Type
   is
      Optimal_Size : Natural;
   begin
      --  Empirically optimized chunk sizes based on benchmarks
      case File_Size is
         --  Small files: use smaller chunks

         when 0 .. 1_000_000 =>
            -- <= 1MB
            Optimal_Size := 64 * SI_KB;           -- 64KB

         when 1_000_001 .. 10_000_000 =>
            -- <= 10MB
            Optimal_Size := 256 * SI_KB;          -- 256KB

            --  Medium files: optimized for 16MB chunks

         when 10_000_001 .. 50_000_000 =>
            -- <= 50MB
            Optimal_Size := 2 * SI_MB;            -- 2MB

         when 50_000_001 .. 500_000_000 =>
            -- 50MB-500MB
            Optimal_Size := 16 * SI_MB;           -- 16MB (optimized)

            --  Large files: balance throughput and memory

         when 500_000_001 .. 2_000_000_000 =>
            -- 500MB-2GB
            Optimal_Size := 64 * SI_MB;           -- 64MB

            --  Huge files: maximum throughput

         when others =>
            -- > 2GB
            Optimal_Size := 128 * SI_MB;          -- 128MB
      end case;

      --  Ensure within bounds
      declare
         Clamped_Size : constant Natural :=
           Natural'Max (MIN_SIZE, Natural'Min (Optimal_Size, MAX_SIZE));
      begin
         return (Ada.Finalization.Controlled with Bytes_Value => Clamped_Size);
      end;
   end Optimal_For_File_Size;

   -- --------------------------
   --  Chunks_Needed_For_File
   -- --------------------------

   function Chunks_Needed_For_File
     (Size : Chunk_Size_Type; File_Size : Long_Long_Integer)
      return Long_Long_Integer is
   begin
      if File_Size = 0 then
         return 0;
      end if;

      --  Round up division
      return
        (File_Size + Long_Long_Integer (Size.Bytes_Value) - 1)
        / Long_Long_Integer (Size.Bytes_Value);
   end Chunks_Needed_For_File;

   -- ----------------------
   --  Is_Optimal_For_File
   -- ----------------------

   function Is_Optimal_For_File
     (Size : Chunk_Size_Type; File_Size : Long_Long_Integer) return Boolean
   is
      Optimal : constant Chunk_Size_Type := Optimal_For_File_Size (File_Size);
   begin
      return Size.Bytes_Value = Optimal.Bytes_Value;
   end Is_Optimal_For_File;

   -- ---------
   --  Image
   -- ---------

   function Image (Size : Chunk_Size_Type) return String is
      Bytes_Val : constant Natural := Size.Bytes_Value;
   begin
      if Bytes_Val >= SI_MB then
         declare
            MB     : constant Float := Float (Bytes_Val) / Float (SI_MB);
            MB_Str : constant String := MB'Image;
         begin
            --  Format as X.XMB
            return MB_Str (MB_Str'First + 1 .. MB_Str'Last) & "MB";
         end;
      elsif Bytes_Val >= SI_KB then
         declare
            KB     : constant Float := Float (Bytes_Val) / Float (SI_KB);
            KB_Str : constant String := KB'Image;
         begin
            --  Format as X.XKB
            return KB_Str (KB_Str'First + 1 .. KB_Str'Last) & "KB";
         end;
      else
         return Bytes_Val'Image (2 .. Bytes_Val'Image'Last) & "B";
      end if;
   end Image;

   -- -------
   --  "="
   -- -------

   overriding
   function "=" (Left, Right : Chunk_Size_Type) return Boolean is
   begin
      return Left.Bytes_Value = Right.Bytes_Value;
   end "=";

   -- -------
   --  "<"
   -- -------

   function "<" (Left, Right : Chunk_Size_Type) return Boolean is
   begin
      return Left.Bytes_Value < Right.Bytes_Value;
   end "<";

   -- --------
   --  "<="
   -- --------

   function "<=" (Left, Right : Chunk_Size_Type) return Boolean is
   begin
      return Left.Bytes_Value <= Right.Bytes_Value;
   end "<=";

   -- -------
   --  ">"
   -- -------

   function ">" (Left, Right : Chunk_Size_Type) return Boolean is
   begin
      return Left.Bytes_Value > Right.Bytes_Value;
   end ">";

   -- --------
   --  ">="
   -- --------

   function ">=" (Left, Right : Chunk_Size_Type) return Boolean is
   begin
      return Left.Bytes_Value >= Right.Bytes_Value;
   end ">=";

end Pipelib.Core.Domain.Value_Objects.Chunk_Size;
