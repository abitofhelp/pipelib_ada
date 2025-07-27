--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.Algorithm - Algorithm Value Object
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Type-safe algorithm specification with validation and categorization.
--   Supports compression, encryption, hashing, and custom algorithms.
--   =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Pipelib.Core.Domain.Value_Objects.Algorithm is

   --  Algorithm categories
   type Algorithm_Category is (Compression, Encryption, Hashing, Custom);

   --  Main algorithm type
   type Algorithm_Type is tagged private;

   --  Forward declaration for validation
   function Is_Valid (Self : Algorithm_Type) return Boolean;

   --  Constructor with validation (returns tagged type directly)
   function Create (Name : String) return Algorithm_Type
     with Pre => Name'Length > 0 and then Is_Valid_Format (Name);

   --  Get algorithm name
   function Name (Self : Algorithm_Type) return String
     with Inline;

   --  Get algorithm category
   function Category (Self : Algorithm_Type) return Algorithm_Category;

   --  Check if name has valid format (lowercase letters, digits, hyphens)
   function Is_Valid_Format (Name : String) return Boolean
     with Post => Is_Valid_Format'Result =
       (for all C of Name =>
          C in 'a' .. 'z' or C in '0' .. '9' or C = '-');

   --  Predefined compression algorithms
   function Brotli return Algorithm_Type
     with Post => Name (Brotli'Result) = "brotli" and
                  Category (Brotli'Result) = Compression;

   function Gzip return Algorithm_Type
     with Post => Name (Gzip'Result) = "gzip" and
                  Category (Gzip'Result) = Compression;

   function Zstd return Algorithm_Type
     with Post => Name (Zstd'Result) = "zstd" and
                  Category (Zstd'Result) = Compression;

   function Lz4 return Algorithm_Type
     with Post => Name (Lz4'Result) = "lz4" and
                  Category (Lz4'Result) = Compression;

   --  Predefined encryption algorithms
   function AES_256_GCM return Algorithm_Type
     with Post => Name (AES_256_GCM'Result) = "aes-256-gcm" and
                  Category (AES_256_GCM'Result) = Encryption;

   function ChaCha20_Poly1305 return Algorithm_Type
     with Post => Name (ChaCha20_Poly1305'Result) = "chacha20-poly1305" and
                  Category (ChaCha20_Poly1305'Result) = Encryption;

   --  Predefined hashing algorithms
   function SHA256 return Algorithm_Type
     with Post => Name (SHA256'Result) = "sha256" and
                  Category (SHA256'Result) = Hashing;

   function SHA512 return Algorithm_Type
     with Post => Name (SHA512'Result) = "sha512" and
                  Category (SHA512'Result) = Hashing;

   function Blake3 return Algorithm_Type
     with Post => Name (Blake3'Result) = "blake3" and
                  Category (Blake3'Result) = Hashing;

   --  Equality operator
   overriding function "=" (Left, Right : Algorithm_Type) return Boolean;

   --  String representation
   function Image (Self : Algorithm_Type) return String is
     (Name (Self) & " (" & Category (Self)'Image & ")");

private
   type Algorithm_Type is tagged record
      Name_Value : Unbounded_String;
   end record
     with Type_Invariant => Is_Valid (Algorithm_Type);

   --  Validation function for type invariant
   function Is_Valid (Self : Algorithm_Type) return Boolean is
     (Length (Self.Name_Value) > 0 and then
      Is_Valid_Format (To_String (Self.Name_Value)));

end Pipelib.Core.Domain.Value_Objects.Algorithm;
