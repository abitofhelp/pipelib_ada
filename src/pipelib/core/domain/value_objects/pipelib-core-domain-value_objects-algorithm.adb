--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.Algorithm - Implementation
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--   =============================================================================

pragma Ada_2022;

package body Pipelib.Core.Domain.Value_Objects.Algorithm is

   -- ----------
   --  Create
   -- ----------

   function Create (Name : String) return Algorithm_Type is
   begin
      if not Is_Valid_Format (Name) then
         raise Constraint_Error with "Invalid algorithm name format: " & Name;
      end if;

      return (Name_Value => To_Unbounded_String (Name));
   end Create;

   -- --------
   --  Name
   -- --------

   function Name (Self : Algorithm_Type) return String is
   begin
      return To_String (Self.Name_Value);
   end Name;

   -- ------------
   --  Category
   -- ------------

   function Category (Self : Algorithm_Type) return Algorithm_Category is
      Algo_Name : constant String := Name (Self);
   begin
      --  Compression algorithms
      if Algo_Name in "brotli" | "gzip" | "zstd" | "lz4" | "zlib" | "deflate"
      then
         return Compression;

         --  Encryption algorithms
      elsif Algo_Name in "aes-256-gcm" | "chacha20-poly1305" | "aes-128-gcm"
      then
         return Encryption;

         --  Hashing algorithms
      elsif Algo_Name in "sha256" | "sha512" | "blake3" | "sha1" | "md5" then
         return Hashing;

         --  Custom algorithms
      else
         return Custom;
      end if;
   end Category;

   -- --------------------
   --  Is_Valid_Format
   -- --------------------

   function Is_Valid_Format (Name : String) return Boolean is
   begin
      if Name'Length = 0 then
         return False;
      end if;

      for C of Name loop
         if not (C in 'a' .. 'z' or C in '0' .. '9' or C = '-') then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Format;

   -- ----------
   --  Brotli
   -- ----------

   function Brotli return Algorithm_Type is
   begin
      return (Name_Value => To_Unbounded_String ("brotli"));
   end Brotli;

   -- --------
   --  Gzip
   -- --------

   function Gzip return Algorithm_Type is
   begin
      return (Name_Value => To_Unbounded_String ("gzip"));
   end Gzip;

   -- --------
   --  Zstd
   -- --------

   function Zstd return Algorithm_Type is
   begin
      return (Name_Value => To_Unbounded_String ("zstd"));
   end Zstd;

   -- -------
   --  Lz4
   -- -------

   function Lz4 return Algorithm_Type is
   begin
      return (Name_Value => To_Unbounded_String ("lz4"));
   end Lz4;

   -- ----------------
   --  AES_256_GCM
   -- ----------------

   function AES_256_GCM return Algorithm_Type is
   begin
      return (Name_Value => To_Unbounded_String ("aes-256-gcm"));
   end AES_256_GCM;

   -- ----------------------
   --  ChaCha20_Poly1305
   -- ----------------------

   function ChaCha20_Poly1305 return Algorithm_Type is
   begin
      return (Name_Value => To_Unbounded_String ("chacha20-poly1305"));
   end ChaCha20_Poly1305;

   -- ----------
   --  SHA256
   -- ----------

   function SHA256 return Algorithm_Type is
   begin
      return (Name_Value => To_Unbounded_String ("sha256"));
   end SHA256;

   -- ----------
   --  SHA512
   -- ----------

   function SHA512 return Algorithm_Type is
   begin
      return (Name_Value => To_Unbounded_String ("sha512"));
   end SHA512;

   -- ----------
   --  Blake3
   -- ----------

   function Blake3 return Algorithm_Type is
   begin
      return (Name_Value => To_Unbounded_String ("blake3"));
   end Blake3;

   -- -----
   --  "="
   -- -----

   overriding
   function "=" (Left, Right : Algorithm_Type) return Boolean is
   begin
      return Left.Name_Value = Right.Name_Value;
   end "=";

end Pipelib.Core.Domain.Value_Objects.Algorithm;
