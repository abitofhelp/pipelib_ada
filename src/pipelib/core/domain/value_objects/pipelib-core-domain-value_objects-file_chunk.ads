--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.File_Chunk - File Chunk Value Object
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   Immutable chunk of data for processing within a pipeline system.
--   Follows Domain-Driven Design principles and ensures data integrity.
--
--   MEMORY MANAGEMENT WITH ADA 2022 OWNERSHIP:
--   =========================================
--   This type uses Ada 2022 ownership features for safe memory management:
--
--   1. OWNERSHIP MODEL:
--      - Each File_Chunk_Type owns its Data (Stream_Element_Array)
--      - Ownership is exclusive - no sharing of data between chunks
--      - Memory is automatically freed when chunk goes out of scope
--
--   2. COPY SEMANTICS:
--      - Assignment creates a deep copy (implemented in Adjust)
--      - Each copy owns its own data independently
--      - No risk of double-free or use-after-free
--
--   3. PERFORMANCE CONSIDERATIONS:
--      - Chunks can be large (up to 512MB)
--      - Deep copying is expensive for large chunks
--      - Use 'renames' or 'access' parameters to avoid copies
--      - Consider using 'Move' semantics when ownership transfer is intended
--
--   4. BEST PRACTICES:
--      - Prefer passing chunks as 'in' parameters (no copy)
--      - Use 'access' parameters when mutation is needed
--      - Avoid unnecessary assignments
--      - Let chunks go out of scope naturally for cleanup
--
--   5. TASK SAFETY:
--      - Read operations are task-safe
--      - Write operations require external synchronization
--      - Each task should own its chunks independently
--
--   6. USAGE EXAMPLES:
--      -- Efficient parameter passing (no copy):
--      procedure Process (Chunk : in File_Chunk_Type);
--
--      -- When mutation is needed (still no copy):
--      procedure Update (Chunk : access File_Chunk_Type);
--
--      -- Efficient ownership transfer:
--      declare
--         Source : File_Chunk_Type := Create (...);
--         Target : File_Chunk_Type;
--      begin
--         Move (Target, Source);  -- Source now has null data
--      end;
--
--      -- Avoid expensive copies:
--      Chunk_Ref : File_Chunk_Type renames Some_Chunk;  -- No copy
--   =============================================================================

pragma Ada_2022;

with Ada.Streams;                                  use Ada.Streams;
with Ada.Calendar;
with Ada.Finalization;
with Ada.Strings.Unbounded;                        use Ada.Strings.Unbounded;
with Pipelib.Core.Domain.Value_Objects.Chunk_Size;
use Pipelib.Core.Domain.Value_Objects.Chunk_Size;
with Pipelib.Core.Domain.Constants;
with Abohlib.Core.Domain.Result;

package Pipelib.Core.Domain.Value_Objects.File_Chunk is

   use Pipelib.Core.Domain.Constants;

   --  Forward declaration
   --  Note: We keep this as a non-limited controlled type to support
   --  value semantics needed by pipeline stages. However, users should
   --  be aware that assignment causes deep copying of potentially large data.
   type File_Chunk_Type is new Ada.Finalization.Controlled with private
   with Type_Invariant => Is_Valid_Chunk (File_Chunk_Type);

   --  Access type for stream element arrays
   type Stream_Element_Array_Access is access Stream_Element_Array;

   --  Maximum chunk data size (512MB max)
   MAX_CHUNK_DATA_SIZE : constant := 512 * 1_000_000;

   --  Exception-based error handling (temporary until Result pattern can be properly implemented)
   File_Chunk_Error : exception;

   package Checksum_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Unbounded_String,  -- SHA-256 hex string
        Err_Type => Unbounded_String);

   package Validation_Result is new
     Abohlib.Core.Domain.Result.Result_Package
       (Ok_Type  => Boolean,
        Err_Type => Unbounded_String);

   --  ## Primary Constructor
   --
   --  Creates a new file chunk with automatically generated UUID identifier.
   --  This is the most commonly used constructor for creating chunks from data.
   --
   --  ### Parameters
   --  * `Sequence_Number` - Sequential position of this chunk in the overall file (0-based)
   --  * `Offset` - Byte offset from the beginning of the source file where this chunk starts
   --  * `Data` - The actual data content as a stream element array
   --  * `Is_Final` - True if this is the last chunk in the file/stream
   --
   --  ### Memory Management
   --  The function creates a deep copy of the provided data array. The original
   --  data array remains owned by the caller and can be safely deallocated after
   --  this call returns.
   --
   --  ### Performance Notes
   --  This constructor performs a memory allocation and data copy. For large chunks
   --  (>1MB), consider using `Create_From_Access` if you can transfer ownership
   --  of the data to avoid the copy overhead.
   --
   --  ### Examples
   --  ```ada
   --  -- Create a small chunk from a string
   --  declare
   --     Text : constant String := "Hello, World!";
   --     Data : Stream_Element_Array (1 .. Text'Length);
   --  begin
   --     for I in Text'Range loop
   --        Data (Stream_Element_Offset (I)) := Character'Pos (Text (I));
   --     end loop;
   --
   --     Chunk : constant File_Chunk_Type := Create (
   --        Sequence_Number => 0,
   --        Offset => 0,
   --        Data => Data,
   --        Is_Final => True
   --     );
   --  end;
   --
   --  -- Create chunk from file data
   --  declare
   --     Buffer : Stream_Element_Array (1 .. 64_000);
   --     Last : Stream_Element_Offset;
   --  begin
   --     Read_File_Data (Buffer, Last);
   --
   --     Chunk : constant File_Chunk_Type := Create (
   --        Sequence_Number => Chunk_Index,
   --        Offset => Long_Long_Integer (Chunk_Index * 64_000),
   --        Data => Buffer (1 .. Last),
   --        Is_Final => Last < Buffer'Last
   --     );
   --  end;
   --  ```
   --
   --  ### Error Conditions
   --  * Raises `Constraint_Error` if `Data'Length = 0`
   --  * Raises `Constraint_Error` if `Data'Length > MAX_CHUNK_DATA_SIZE`
   --  * Raises `Constraint_Error` if `Offset < 0`
   --
   --  ### Thread Safety
   --  This function is thread-safe and can be called concurrently from multiple tasks.
   function Create
     (Sequence_Number : Sequence_Number_Type;
      Offset          : File_Position_Type;
      Data            : Stream_Element_Array;
      Is_Final        : Boolean) return File_Chunk_Type
   with
     Pre =>
       Data'Length > 0 and Data'Length <= MAX_CHUNK_DATA_SIZE and Offset >= 0,
     Post =>
       not Is_Empty (Create'Result)
       and then File_Chunk.Sequence_Number (Create'Result) = Sequence_Number
       and then File_Chunk.Offset (Create'Result) = Offset
       and then Natural (Value (Size (Create'Result))) = Data'Length
       and then File_Chunk.Is_Final (Create'Result) = Is_Final
       and then Id (Create'Result)'Length > 0;

   --  ## Constructor with Pre-calculated Checksum
   --
   --  Creates a new file chunk with a pre-calculated SHA-256 checksum.
   --  Use this when you already have a verified checksum for the data,
   --  which avoids the computational cost of recalculating it.
   --
   --  ### When to Use This
   --  * When receiving chunks from a network source that includes checksums
   --  * When reading chunks from a file format that stores checksums
   --  * When you've already calculated the checksum for verification purposes
   --
   --  ### Parameters
   --  * `Sequence_Number` - Sequential position of this chunk (0-based)
   --  * `Offset` - Byte offset from file beginning
   --  * `Data` - The actual data content
   --  * `Checksum` - Pre-calculated SHA-256 hash as 64-character hex string
   --  * `Is_Final` - True if this is the last chunk
   --
   --  ### Checksum Format
   --  The checksum must be exactly 64 characters containing only hexadecimal
   --  digits (0-9, a-f, A-F). This represents a SHA-256 hash in lowercase
   --  hexadecimal format.
   --
   --  ### Examples
   --  ```ada
   --  -- Create chunk with known checksum
   --  declare
   --     Data : constant Stream_Element_Array := Load_Data_From_Network;
   --     Hash : constant String := "a3b2c1d4e5f6789012345678901234567890abcdef1234567890abcdef123456";
   --  begin
   --     Chunk : constant File_Chunk_Type := Create_With_Checksum (
   --        Sequence_Number => 42,
   --        Offset => 2_688_000,  -- 42 * 64KB
   --        Data => Data,
   --        Checksum => Hash,
   --        Is_Final => False
   --     );
   --
   --     -- Verify the provided checksum matches the data
   --     if not Validation_Result.Get_Ok (Verify_Checksum (Chunk)) then
   --        Put_Line ("Warning: Checksum mismatch detected!");
   --     end if;
   --  end;
   --  ```
   --
   --  ### Performance Benefits
   --  This constructor is faster than `Create` followed by `Calculate_And_Set_Checksum`
   --  because it skips the SHA-256 calculation, which can be expensive for large chunks.
   function Create_With_Checksum
     (Sequence_Number : Sequence_Number_Type;
      Offset          : File_Position_Type;
      Data            : Stream_Element_Array;
      Checksum        : String;
      Is_Final        : Boolean) return File_Chunk_Type
   with
     Pre =>
       Data'Length > 0
       and Data'Length <= MAX_CHUNK_DATA_SIZE
       and Offset >= 0
       and Checksum'Length = 64,
     Post =>
       not Is_Empty (Create_With_Checksum'Result)
       and then File_Chunk.Sequence_Number (Create_With_Checksum'Result)
                = Sequence_Number
       and then File_Chunk.Offset (Create_With_Checksum'Result) = Offset
       and then Natural (Value (Size (Create_With_Checksum'Result)))
                = Data'Length
       and then File_Chunk.Is_Final (Create_With_Checksum'Result) = Is_Final
       and then Id (Create_With_Checksum'Result)'Length > 0
       and then File_Chunk.Checksum (Create_With_Checksum'Result) = Checksum;

   --  ## Zero-Copy Constructor (Ownership Transfer)
   --
   --  Creates a new file chunk by taking ownership of existing data.
   --  This is the most efficient constructor as it avoids copying data,
   --  but requires careful memory management.
   --
   --  ### Ownership Rules
   --  **CRITICAL**: After calling this function, the chunk owns the data array.
   --  The caller must NEVER access or deallocate the data pointer again.
   --  The chunk will automatically deallocate the memory when it's destroyed.
   --
   --  ### When to Use This
   --  * When you have large data arrays (>1MB) and want to avoid copy overhead
   --  * When reading data from memory-mapped files
   --  * When receiving data from external sources that allocate memory
   --  * In performance-critical paths where memory allocation is expensive
   --
   --  ### Parameters
   --  * `Sequence_Number` - Sequential position of this chunk (0-based)
   --  * `Offset` - Byte offset from file beginning
   --  * `Data` - Pointer to allocated data array (ownership transfers to chunk)
   --  * `Is_Final` - True if this is the last chunk
   --
   --  ### Memory Safety
   --  This function is marked with `not null` to ensure the data pointer
   --  is valid at call time. However, the caller is responsible for ensuring:
   --  1. The data was allocated with `new` and matches the array bounds
   --  2. No other code will access the data after this call
   --  3. The data pointer is not used elsewhere after ownership transfer
   --
   --  ### Examples
   --  ```ada
   --  -- Efficient creation from large buffer
   --  declare
   --     Large_Buffer : constant Stream_Element_Array_Access :=
   --        new Stream_Element_Array (1 .. 1_000_000);
   --  begin
   --     -- Fill buffer with data
   --     Read_Large_File_Block (Large_Buffer.all);
   --
   --     -- Transfer ownership to chunk (no copy!)
   --     Chunk : constant File_Chunk_Type := Create_From_Access (
   --        Sequence_Number => 1,
   --        Offset => 1_000_000,
   --        Data => Large_Buffer,  -- Ownership transferred
   --        Is_Final => False
   --     );
   --
   --     -- IMPORTANT: Do NOT use Large_Buffer after this point!
   --     -- The chunk now owns the memory and will free it automatically
   --  end;
   --
   --  -- Pattern for memory-mapped files
   --  declare
   --     Mapped_Data : constant Stream_Element_Array_Access :=
   --        Map_File_Region (File, Offset => 64000, Length => 64000);
   --  begin
   --     Chunk : constant File_Chunk_Type := Create_From_Access (
   --        Sequence_Number => 1,
   --        Offset => 64000,
   --        Data => Mapped_Data,
   --        Is_Final => Check_If_Final (File, 64000 + 64000)
   --     );
   --  end;
   --  ```
   --
   --  ### Performance Impact
   --  This constructor is O(1) - it only copies the pointer, not the data.
   --  Compare this to `Create()` which is O(n) where n is the data size.
   --  For a 1MB chunk, this can be 1000x faster than copying.
   function Create_From_Access
     (Sequence_Number : Sequence_Number_Type;
      Offset          : File_Position_Type;
      Data            : not null Stream_Element_Array_Access;
      Is_Final        : Boolean) return File_Chunk_Type
   with
     Pre =>
       Data /= null
       and Data.all'Length > 0
       and Data.all'Length <= MAX_CHUNK_DATA_SIZE
       and Offset >= 0,
     Post =>
       not Is_Empty (Create_From_Access'Result)
       and then File_Chunk.Sequence_Number (Create_From_Access'Result)
                = Sequence_Number
       and then File_Chunk.Offset (Create_From_Access'Result) = Offset
       and then Natural (Value (Size (Create_From_Access'Result)))
                = Data.all'Length
       and then File_Chunk.Is_Final (Create_From_Access'Result) = Is_Final
       and then Id (Create_From_Access'Result)'Length > 0;

   --  Builder methods (create new instances)
   function With_Checksum
     (Chunk : File_Chunk_Type; Checksum : String) return File_Chunk_Type
   with Pre => not Is_Empty (Chunk) and Checksum'Length = 64;
   --  Returns new chunk with checksum added

   function Calculate_And_Set_Checksum
     (Chunk : File_Chunk_Type) return File_Chunk_Type
   with Pre => not Is_Empty (Chunk);
   --  Returns new chunk with calculated SHA-256 checksum

   --  ## Accessor Functions
   --
   --  These functions provide read-only access to chunk properties.
   --  All accessors are marked `Inline` for performance and have
   --  contracts to ensure safe usage.

   --  ### Unique Identifier Access
   --
   --  Returns the unique identifier for this chunk as a ULID string.
   --  Each chunk gets a unique identifier when created, which is useful
   --  for tracking, logging, and debugging purposes.
   --
   --  #### Returns
   --  A string representation of the ULID (Universally Unique Lexicographically
   --  Sortable Identifier). The string will always have a length > 0.
   --
   --  #### Example
   --  ```ada
   --  Put_Line ("Processing chunk: " & Id (Chunk));
   --  ```
   function Id (Chunk : File_Chunk_Type) return String
   with Pre => not Is_Empty (Chunk), Post => Id'Result'Length > 0, Inline;

   --  ### Sequence Position Access
   --
   --  Returns the sequential position of this chunk within the overall file or stream.
   --  Sequence numbers typically start at 0 and increment for each chunk.
   --  This is essential for reassembling chunks in the correct order.
   --
   --  #### Returns
   --  Natural number representing the chunk's position (0-based indexing)
   --
   --  #### Example
   --  ```ada
   --  if Sequence_Number (Chunk) = 0 then
   --     Put_Line ("This is the first chunk");
   --  end if;
   --  ```
   function Sequence_Number (Chunk : File_Chunk_Type) return Sequence_Number_Type
   with
     Pre => not Is_Empty (Chunk),
     Post => Sequence_Number'Result >= 0,
     Inline;

   --  ### File Offset Access
   --
   --  Returns the byte offset from the beginning of the source file where
   --  this chunk's data starts. This is crucial for random-access file
   --  operations and parallel processing where chunks may be processed
   --  out of order.
   --
   --  #### Returns
   --  Byte offset as Long_Long_Integer (supports files > 2GB)
   --
   --  #### Example
   --  ```ada
   --  Put_Line ("Chunk starts at byte position:" & Offset (Chunk)'Image);
   --  ```
   function Offset (Chunk : File_Chunk_Type) return File_Position_Type
   with Pre => not Is_Empty (Chunk), Post => Offset'Result >= 0, Inline;

   --  ### Size Information Access
   --
   --  Returns the size of the chunk's data as a type-safe Chunk_Size_Type.
   --  This provides validated size information with built-in constraints.
   --
   --  #### Returns
   --  Chunk_Size_Type that guarantees the size is within valid bounds
   --
   --  #### Example
   --  ```ada
   --  if Value (Size (Chunk)) > 1_000_000 then
   --     Put_Line ("Large chunk detected");
   --  end if;
   --  ```
   function Size (Chunk : File_Chunk_Type) return Chunk_Size.Chunk_Size_Type
   with
     Pre => not Is_Empty (Chunk),
     Post => Chunk_Size.Is_Valid (Size'Result),
     Inline;

   --  ### Data Content Access (Copy)
   --
   --  Returns a copy of the chunk's data as a Stream_Element_Array.
   --  Use this when you need to modify the data or when the chunk
   --  might be deallocated while you're using the data.
   --
   --  #### Performance Note
   --  This function creates a copy of the data, which can be expensive
   --  for large chunks. Consider using `Data_Access` if you only need
   --  read-only access to avoid the copy overhead.
   --
   --  #### Returns
   --  A new Stream_Element_Array containing a copy of the chunk's data
   --
   --  #### Example
   --  ```ada
   --  declare
   --     Chunk_Data : constant Stream_Element_Array := Data (Chunk);
   --  begin
   --     -- Safe to use Chunk_Data even if Chunk is deallocated
   --     Process_Data (Chunk_Data);
   --  end;
   --  ```
   function Data (Chunk : File_Chunk_Type) return Stream_Element_Array
   with
     Pre => not Is_Empty (Chunk),
     Post => Data'Result'Length = Data_Length (Chunk),
     Inline;

   --  ### Data Content Access (Zero-Copy)
   --
   --  Returns direct access to the chunk's internal data without copying.
   --  This is the most efficient way to access chunk data when you only
   --  need read-only access. The returned access type points directly
   --  to the chunk's internal storage.
   --
   --  #### Safety Rules
   --  * The returned pointer is valid only as long as the chunk exists
   --  * Do NOT modify the data through this access type
   --  * Do NOT store the pointer beyond the chunk's lifetime
   --  * The chunk owns the memory - do not deallocate it
   --
   --  #### Performance Benefits
   --  Zero copy overhead - O(1) operation regardless of chunk size.
   --  For large chunks (>1MB), this can be significantly faster than `Data()`.
   --
   --  #### Example
   --  ```ada
   --  declare
   --     Data_Ptr : constant Stream_Element_Array_Access := Data_Access (Chunk);
   --  begin
   --     -- Efficient read-only access
   --     Hash := Calculate_Hash (Data_Ptr.all);
   --
   --     -- Do NOT do this:
   --     -- Data_Ptr.all (1) := 42;  -- Violates immutability!
   --     -- Free (Data_Ptr);         -- Chunk owns the memory!
   --  end;
   --  ```
   function Data_Access
     (Chunk : File_Chunk_Type) return not null Stream_Element_Array_Access
   with
     Pre => not Is_Empty (Chunk),
     Post =>
       Data_Access'Result /= null
       and then Data_Access'Result.all'Length = Data_Length (Chunk),
     Inline;

   function Checksum (Chunk : File_Chunk_Type) return String
   with
     Pre => not Is_Empty (Chunk),
     Post =>
       (if Has_Checksum (Chunk) then Checksum'Result'Length = 64
        else Checksum'Result'Length = 0);
   --  Returns empty string if no checksum

   function Has_Checksum (Chunk : File_Chunk_Type) return Boolean
   with Pre => not Is_Empty (Chunk), Inline;

   function Is_Final (Chunk : File_Chunk_Type) return Boolean
   with Pre => not Is_Empty (Chunk), Inline;

   function Created_At (Chunk : File_Chunk_Type) return Ada.Calendar.Time
   with Pre => not Is_Empty (Chunk), Inline;

   function Data_Length (Chunk : File_Chunk_Type) return Natural
   with
     Pre => not Is_Empty (Chunk),
     Post =>
       Data_Length'Result > 0 and Data_Length'Result <= MAX_CHUNK_DATA_SIZE,
     Inline;

   function Is_Empty (Chunk : File_Chunk_Type) return Boolean
   with Inline;

   --  Validation
   function Is_Valid_Chunk (Chunk : File_Chunk_Type) return Boolean
   with Inline;
   --  Type invariant validation for File_Chunk_Type

   function Verify_Checksum
     (Chunk : File_Chunk_Type) return Validation_Result.Result
   with
     Pre => not Is_Empty (Chunk),
     Post =>
       (if Validation_Result.Is_Ok (Verify_Checksum'Result)
        then
          (if not Has_Checksum (Chunk)
           then Validation_Result.Get_Ok (Verify_Checksum'Result) = True
           else True));  -- Checksum validation result is meaningful
   --  Verifies checksum if present, returns Success(True) if no checksum

   function Calculate_Checksum
     (Data : Stream_Element_Array) return Checksum_Result.Result
   with
     Pre => Data'Length > 0,
     Post =>
       (if Checksum_Result.Is_Ok (Calculate_Checksum'Result)
        then
          To_String (Checksum_Result.Get_Ok (Calculate_Checksum'Result))'Length
          = 64);
   --  Calculates SHA-256 checksum of data

   --  Comparison
   overriding
   function "=" (Left, Right : File_Chunk_Type) return Boolean;
   --  Compares chunks based on sequence number, offset, data content, checksum, and final flag
   --  Note: Does not compare Id or Created_At fields

   --  String representation
   function Image (Chunk : File_Chunk_Type) return String
   with
     Pre => not Is_Empty (Chunk),
     Post => Image'Result'Length > 0 and Image'Result'First = 1;

   --  ## Efficient Ownership Transfer Operation
   --
   --  Transfers ownership of chunk data from Source to Target without copying
   --  the actual data. This is the most efficient way to transfer large chunks
   --  between different parts of your application.
   --
   --  ### What This Does
   --  * Copies all metadata (ID, sequence number, offset, checksum, etc.) to Target
   --  * Transfers ownership of the data pointer from Source to Target
   --  * Sets Source to an empty state (no data, but safe to use)
   --  * No memory allocation or data copying occurs
   --
   --  ### Performance Benefits
   --  This operation is O(1) regardless of chunk size. Moving a 500MB chunk
   --  takes the same time as moving a 1KB chunk. Compare this to assignment
   --  which would copy all 500MB of data.
   --
   --  ### When to Use This
   --  * Transferring chunks between processing stages
   --  * Moving chunks into collections or queues
   --  * Returning chunks from functions without copying
   --  * Any time you want to transfer ownership rather than share it
   --
   --  ### State After Move
   --  * **Target**: Contains all data and metadata from Source
   --  * **Source**: Becomes empty (Is_Empty returns True), but remains valid
   --  * **Memory**: No new allocation, no deallocation, just pointer transfer
   --
   --  ### Safety Guarantees
   --  * Source cannot be accidentally used after move (Is_Empty will be True)
   --  * No memory leaks (Target takes full ownership responsibility)
   --  * No double-free errors (Source no longer owns the data)
   --  * Target gets a fully valid chunk with all contracts satisfied
   --
   --  ### Examples
   --  ```ada
   --  -- Efficient chunk transfer in pipeline
   --  procedure Process_Stage (Input : in out File_Chunk_Type;
   --                          Output : out File_Chunk_Type) is
   --     Processed_Data : Stream_Element_Array := Transform (Data (Input));
   --     Result_Chunk : File_Chunk_Type := Create (
   --        Sequence_Number (Input), Offset (Input), Processed_Data, Is_Final (Input));
   --  begin
   --     -- Transfer ownership efficiently
   --     Move (Target => Output, Source => Result_Chunk);
   --     -- Result_Chunk is now empty, Input unchanged
   --  end Process_Stage;
   --
   --  -- Moving chunk into a vector without copying
   --  declare
   --     Chunks : File_Chunk_Vector;
   --     Large_Chunk : File_Chunk_Type := Create_Large_Chunk;
   --     Empty_Chunk : File_Chunk_Type;
   --  begin
   --     Move (Target => Empty_Chunk, Source => Large_Chunk);
   --     Chunks.Append (Empty_Chunk);  -- Efficient move into vector
   --
   --     -- Large_Chunk is now empty, but still valid
   --     pragma Assert (Is_Empty (Large_Chunk));
   --  end;
   --
   --  -- Efficient return from function
   --  function Create_And_Process_Chunk return File_Chunk_Type is
   --     Working_Chunk : File_Chunk_Type := Create (...);
   --     Result : File_Chunk_Type;
   --  begin
   --     -- Do processing...
   --
   --     Move (Target => Result, Source => Working_Chunk);
   --     return Result;  -- No copy on return
   --  end Create_And_Process_Chunk;
   --  ```
   --
   --  ### Memory Management Impact
   --  This operation maintains Ada's memory safety while providing zero-copy
   --  semantics. The controlled type's finalization ensures proper cleanup
   --  regardless of which chunk currently owns the data.
   procedure Move
     (Target : out File_Chunk_Type; Source : in out File_Chunk_Type)
   with
     Pre => not Is_Empty (Source),
     Post =>
       Is_Empty (Source)
       and not Is_Empty (Target)
       and Data_Length (Target) = Data_Length (Source)'Old;

   --  Validation subtypes with predicates
   function Is_Valid_Checksum (S : String) return Boolean
   with
     Post =>
       Is_Valid_Checksum'Result
       = (S'Length = 64
          and then (for all C of S
                    => C in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'));

   subtype Valid_Checksum_String is String
   with Dynamic_Predicate => Is_Valid_Checksum (Valid_Checksum_String);

   subtype Valid_Chunk_Size is Natural
   with
     Dynamic_Predicate =>
       Valid_Chunk_Size > 0 and Valid_Chunk_Size <= MAX_CHUNK_DATA_SIZE;

   subtype Valid_Offset is File_Position_Type
   with Dynamic_Predicate => Valid_Offset >= 0;

   --  Error messages
   EMPTY_DATA_ERROR       : constant String := "Chunk data cannot be empty";
   SIZE_EXCEEDED_ERROR    : constant String :=
     "Chunk data exceeds maximum size";
   INVALID_CHECKSUM_ERROR : constant String :=
     "Invalid checksum format (must be 64 hex chars)";

   --  Controlled type operations (must be declared before private part)
   overriding
   procedure Adjust (Object : in out File_Chunk_Type);
   overriding
   procedure Finalize (Object : in out File_Chunk_Type);

private
   --  Access type moved to public part for Create_From_Access function

   --  File_Chunk_Type uses Ada 2022 ownership for memory management
   --  IMPORTANT: Memory Management Guidelines:
   --  1. The Data field owns the allocated memory
   --  2. Assignment creates a new copy (deep copy in Adjust)
   --  3. Memory is automatically freed when chunk goes out of scope
   --  4. Use 'Move' aspect when transferring ownership is desired
   --  5. The type is task-safe for read operations only
   type File_Chunk_Type is new Ada.Finalization.Controlled with record
      Id              : Unbounded_String;        -- ULID as string
      Sequence_Number : Sequence_Number_Type;
      Offset          : File_Position_Type;
      Size            : Chunk_Size.Chunk_Size_Type;
      Data            : Stream_Element_Array_Access;
      Checksum        : Unbounded_String;        -- Empty if no checksum
      Is_Final        : Boolean;
      Created_At      : Ada.Calendar.Time;
   end record;

   --  Type invariant implementation
   function Is_Valid_Chunk (Chunk : File_Chunk_Type) return Boolean
   is ((if Chunk.Data /= null
        then Chunk.Data.all'Length <= MAX_CHUNK_DATA_SIZE)
       and then Chunk.Offset >= 0
       and then (if Length (Chunk.Checksum) > 0
                 then Length (Chunk.Checksum) = 64));

end Pipelib.Core.Domain.Value_Objects.File_Chunk;
