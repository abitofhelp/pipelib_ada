--  =============================================================================
--  Pipelib.Core.Domain.Entities.Chunk - Chunk Entity
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  ## Chunk Entity - Stateful Data Processing Unit
--
--  Represents a chunk of data being processed through the pipeline.
--  This is a Domain-Driven Design (DDD) entity with identity (chunk number)
--  and mutable state that tracks the chunk's progress through various
--  processing stages.
--
--  ### Entity vs Value Object
--  Unlike File_Chunk_Type (value object), Chunk_Type is an entity:
--  * **Identity**: Each chunk has a unique number that defines its identity
--  * **Mutable State**: Chunks transition through processing states
--  * **Lifecycle**: Chunks are created, processed, and can be reused
--  * **Reference Equality**: Two chunks are equal if they have the same number
--
--  ### State Machine
--  Chunks follow a well-defined state machine with retry capabilities:
--
--  ```
--  Created Ã¢ÂÂ Reading Ã¢ÂÂ Read Ã¢ÂÂ Processing Ã¢ÂÂ Processed Ã¢ÂÂ Writing Ã¢ÂÂ Written
--     Ã¢ÂÂ         Ã¢ÂÂ        Ã¢ÂÂ        Ã¢ÂÂ           Ã¢ÂÂ          Ã¢ÂÂ
--     +----------+---------+---------+------------+-----------+
--           (Retry paths for error recovery)
--  ```
--
--  ### Thread Safety
--  Chunk entities are NOT thread-safe. Each chunk should be owned by a
--  single task at a time. Use message passing or protected types to
--  coordinate chunk access between tasks.
--
--  ### Memory Management
--  Chunks use zero-copy operations through ownership transfer:
--  * Set_Data transfers ownership of memory to the chunk
--  * The chunk is responsible for deallocating memory when reset/destroyed
--  * Data pointers become null after ownership transfer
--
--  ### Performance Considerations
--  * State transitions are O(1) operations
--  * Data operations transfer ownership, avoiding copies
--  * Chunks can be pooled and reused to reduce allocation overhead
--  * Inline functions provide efficient access to properties
--  =============================================================================

pragma Ada_2022;

with Ada.Streams; use Ada.Streams;
with Pipelib.Core.Domain.Value_Objects.Chunk_Size;

package Pipelib.Core.Domain.Entities.Chunk is

   --  ## Processing States
   --
   --  Defines the lifecycle states a chunk transitions through during processing.
   --  Each state represents a specific phase in the data processing pipeline.
   --
   --  ### State Descriptions
   --  * `Created` - Initial state, chunk allocated but no data assigned
   --  * `Reading` - Currently reading data from source (I/O in progress)
   --  * `Read` - Data successfully read and available for processing
   --  * `Processing` - Data being transformed/processed by pipeline stage
   --  * `Processed` - Processing complete, ready for output
   --  * `Writing` - Currently writing data to destination (I/O in progress)
   --  * `Written` - Final state, data successfully written (terminal state)
   --
   --  ### Retry Semantics
   --  Some states allow transition back to earlier states for retry scenarios:
   --  * `Reading` Ã¢ÂÂ `Created` (retry read operation)
   --  * `Processing` Ã¢ÂÂ `Read` (retry processing operation)
   --  * `Writing` Ã¢ÂÂ `Processed` (retry write operation)
   type Chunk_State is
     (Created, Reading, Read, Processing, Processed, Writing, Written);

   --  ## Chunk Entity Type
   --
   --  Tagged type representing a chunk entity with identity and mutable state.
   --  This is the core entity for tracking data chunks through the pipeline.
   --
   --  ### Identity
   --  Each chunk has a unique number that serves as its identity. Chunks with
   --  the same number are considered the same entity, even if other properties differ.
   --
   --  ### State Management
   --  Chunks maintain processing state and support state transition validation
   --  through contracts. Invalid state transitions are prevented at compile-time
   --  or runtime through precondition checks.
   type Chunk_Type is tagged private;

   --  ## Access Types for Zero-Copy Operations
   --
   --  These access types enable efficient memory management and zero-copy
   --  operations when transferring data ownership between components.
   type Chunk_Access is access all Chunk_Type;
   type Stream_Element_Array_Access is access all Stream_Element_Array;

   --  ## Chunk Factory Function
   --
   --  Creates a new chunk with the specified identity and allocated size.
   --  The chunk starts in the `Created` state with no data assigned.
   --
   --  ### Parameters
   --  * `Number` - Unique identifier for this chunk (0-based sequence)
   --  * `Size` - Pre-allocated size for the chunk's data buffer
   --
   --  ### Initial State
   --  New chunks are created with safe default values:
   --  * State: `Created`
   --  * Data: `null` (no data assigned yet)
   --  * Data_Size: `0` (no actual data)
   --  * Compressed: `False`
   --  * Retry_Count: `0`
   --
   --  ### Usage Patterns
   --  ```ada
   --  -- Create chunk for 64KB data
   --  declare
   --     Chunk_Size : constant Chunk_Size_Type := From_Natural (65536);
   --     My_Chunk : constant Chunk_Type := Create (Number => 42, Size => Chunk_Size);
   --  begin
   --     pragma Assert (Number (My_Chunk) = 42);
   --     pragma Assert (State (My_Chunk) = Created);
   --     pragma Assert (Data (My_Chunk) = null);
   --  end;
   --
   --  -- Create chunks in a loop for pipeline processing
   --  for I in 0 .. 99 loop
   --     Chunks (I) := Create (Number => I, Size => Standard_Chunk_Size);
   --  end loop;
   --  ```
   --
   --  ### Memory Allocation
   --  This function only allocates the chunk entity itself, not the data buffer.
   --  The actual data is assigned later through `Set_Data` when ready.
   function Create
     (Number : Natural;
      Size   : Pipelib.Core.Domain.Value_Objects.Chunk_Size.Chunk_Size_Type)
      return Chunk_Type
   with
     Pre => Pipelib.Core.Domain.Value_Objects.Chunk_Size.Is_Valid (Size),
     Post =>
       Create'Result.Number = Number
       and State (Create'Result) = Created
       and Data (Create'Result) = null
       and Data_Size (Create'Result) = 0
       and not Is_Compressed (Create'Result)
       and Retry_Count (Create'Result) = 0;

   --  ## Identity Access
   --
   --  Returns the unique identifier number for this chunk entity.
   --  The chunk number serves as the primary key and defines the chunk's identity
   --  throughout its lifecycle. Two chunks with the same number are considered
   --  the same entity.
   --
   --  ### Returns
   --  Natural number representing the chunk's unique position in sequence
   --
   --  ### Usage
   --  ```ada
   --  Put_Line ("Processing chunk #" & Number (Chunk)'Image);
   --
   --  -- Sorting chunks by number
   --  if Number (Chunk_A) < Number (Chunk_B) then
   --     Process_First (Chunk_A);
   --  end if;
   --  ```
   function Number (Chunk : Chunk_Type) return Natural
   with Post => Number'Result >= 0, Inline;

   --  ## Current State Access
   --
   --  Returns the current processing state of the chunk. This indicates
   --  what phase of the pipeline the chunk is currently in and what operations
   --  are valid to perform on it.
   --
   --  ### Returns
   --  Current Chunk_State value representing the processing phase
   --
   --  ### Usage for Flow Control
   --  ```ada
   --  case State (Chunk) is
   --     when Created =>
   --        -- Ready to start reading data
   --        Begin_Read_Operation (Chunk);
   --     when Read =>
   --        -- Data available, start processing
   --        Begin_Processing (Chunk);
   --     when Processed =>
   --        -- Processing complete, start writing
   --        Begin_Write_Operation (Chunk);
   --     when Written =>
   --        -- Complete, chunk can be recycled
   --        Return_To_Pool (Chunk);
   --     when others =>
   --        -- I/O operation in progress, wait
   --        null;
   --  end case;
   --  ```
   function State (Chunk : Chunk_Type) return Chunk_State
   with Inline;

   --  ## State Transition Operation
   --
   --  Changes the chunk's processing state to a new value. This operation
   --  is protected by contracts that ensure only valid state transitions
   --  are allowed, preventing invalid pipeline states.
   --
   --  ### Parameters
   --  * `Chunk` - The chunk entity to update (modified in place)
   --  * `State` - New state to transition to
   --
   --  ### Valid Transitions
   --  The state machine enforces these allowed transitions:
   --
   --  **Forward Progress:**
   --  * `Created` Ã¢ÂÂ `Reading` (start reading data)
   --  * `Reading` Ã¢ÂÂ `Read` (reading completed successfully)
   --  * `Read` Ã¢ÂÂ `Processing` (start processing data)
   --  * `Processing` Ã¢ÂÂ `Processed` (processing completed)
   --  * `Processed` Ã¢ÂÂ `Writing` (start writing results)
   --  * `Writing` Ã¢ÂÂ `Written` (writing completed)
   --
   --  **Retry Paths:**
   --  * `Reading` Ã¢ÂÂ `Created` (retry read from beginning)
   --  * `Processing` Ã¢ÂÂ `Read` (retry processing step)
   --  * `Writing` Ã¢ÂÂ `Processed` (retry write operation)
   --
   --  **Skip Transitions:**
   --  * `Read` Ã¢ÂÂ `Writing` (skip processing if not needed)
   --
   --  ### Error Handling
   --  Attempting an invalid state transition will raise a `Constraint_Error`
   --  due to the precondition check. This prevents chunks from entering
   --  inconsistent states that could cause pipeline failures.
   --
   --  ### Examples
   --  ```ada
   --  -- Normal processing flow
   --  Set_State (Chunk, Reading);
   --  -- ... perform I/O operation ...
   --  Set_State (Chunk, Read);
   --
   --  -- Error recovery with retry
   --  begin
   --     Process_Chunk_Data (Chunk);
   --     Set_State (Chunk, Processed);
   --  exception
   --     when Processing_Error =>
   --        Increment_Retry_Count (Chunk);
   --        Set_State (Chunk, Read);  -- Retry from Read state
   --  end;
   --
   --  -- Skip processing for pass-through data
   --  if Not_Requires_Processing (Chunk) then
   --     Set_State (Chunk, Writing);  -- Skip directly to writing
   --  end if;
   --  ```
   --
   --  ### Thread Safety
   --  This operation is NOT thread-safe. Ensure only one task modifies
   --  a chunk's state at a time.
   procedure Set_State (Chunk : in out Chunk_Type; State : Chunk_State)
   with
     Pre => Is_Valid_Transition (Chunk.State, State),
     Post => Chunk.State = State;

   --  ## Data Management - Zero-Copy Operations
   --
   --  These functions provide efficient data access and ownership transfer
   --  without copying large data arrays. They follow Ada's ownership model
   --  to ensure memory safety while maximizing performance.

   --  ### Data Access (Read-Only)
   --
   --  Returns direct access to the chunk's data without copying.
   --  The returned pointer is valid as long as the chunk exists and
   --  has not been reset.
   --
   --  #### Returns
   --  * Access to the chunk's data array, or `null` if no data assigned
   --  * The caller must NOT modify data through this access type
   --  * The caller must NOT deallocate the memory (chunk owns it)
   --
   --  #### Usage
   --  ```ada
   --  declare
   --     Data_Ptr : constant Stream_Element_Array_Access := Data (Chunk);
   --  begin
   --     if Data_Ptr /= null then
   --        Hash := Calculate_Hash (Data_Ptr.all);
   --        Size := Data_Ptr.all'Length;
   --     end if;
   --  end;
   --  ```
   function Data (Chunk : Chunk_Type) return Stream_Element_Array_Access
   with Inline;

   --  ### Data Ownership Transfer (Zero-Copy)
   --
   --  Transfers ownership of a data array to the chunk. This is the most
   --  efficient way to assign data as it avoids copying potentially large
   --  arrays. After this call, the chunk owns the memory and will deallocate
   --  it when the chunk is reset or destroyed.
   --
   --  #### Parameters
   --  * `Chunk` - The chunk entity to receive the data (modified in place)
   --  * `Data` - Pointer to allocated data array (ownership transfers to chunk)
   --
   --  #### Ownership Rules
   --  **CRITICAL**: After calling this procedure:
   --  1. The `Data` parameter becomes `null` (ownership transferred)
   --  2. The chunk now owns the memory and will deallocate it
   --  3. The caller must NEVER access the original data pointer again
   --  4. The caller must NEVER deallocate the memory manually
   --
   --  #### Performance Benefits
   --  This operation is O(1) regardless of data size. Transferring ownership
   --  of a 100MB data array takes the same time as transferring a 1KB array.
   --  Compare this to copying, which would be O(n) where n is the data size.
   --
   --  #### Memory Safety
   --  The operation is memory-safe through Ada's type system:
   --  * The `Data` parameter becomes `null`, preventing accidental reuse
   --  * The chunk takes full responsibility for memory management
   --  * Ada's controlled types ensure proper cleanup during finalization
   --
   --  #### Examples
   --  ```ada
   --  -- Efficient data assignment from I/O operation
   --  declare
   --     Buffer : Stream_Element_Array_Access := new Stream_Element_Array (1 .. 65536);
   --     Chunk : Chunk_Type := Create (Number => 42, Size => From_Natural (65536));
   --  begin
   --     -- Read data into buffer
   --     Read_From_File (File, Buffer.all);
   --
   --     -- Transfer ownership to chunk (no copy!)
   --     Set_Data (Chunk, Buffer);
   --
   --     -- IMPORTANT: Buffer is now null, don't use it!
   --     pragma Assert (Buffer = null);
   --
   --     -- Chunk now owns the data
   --     pragma Assert (Data (Chunk) /= null);
   --     pragma Assert (Data_Size (Chunk) = 65536);
   --  end;
   --
   --  -- Transferring data between chunks efficiently
   --  declare
   --     Source_Chunk : Chunk_Type := ...;  -- Has data
   --     Target_Chunk : Chunk_Type := ...;  -- Empty
   --     Data_Ptr : Stream_Element_Array_Access := Data (Source_Chunk);
   --  begin
   --     -- This would NOT work (read-only access):
   --     -- Set_Data (Target_Chunk, Data_Ptr);  -- Error!
   --
   --     -- Instead, use chunk-level operations or create new data
   --     Transferred_Data : Stream_Element_Array_Access :=
   --        new Stream_Element_Array'(Data (Source_Chunk).all);
   --     Set_Data (Target_Chunk, Transferred_Data);
   --  end;
   --  ```
   --
   --  #### Common Patterns
   --
   --  **Reading from Files:**
   --  ```ada
   --  Buffer : Stream_Element_Array_Access := Allocate_Buffer (Size);
   --  Read_File_Block (File, Buffer.all);
   --  Set_Data (Chunk, Buffer);  -- Transfer ownership
   --  ```
   --
   --  **Memory-Mapped Files:**
   --  ```ada
   --  Mapped_Data : Stream_Element_Array_Access := Map_File_Region (File, Offset, Size);
   --  Set_Data (Chunk, Mapped_Data);  -- Transfer ownership
   --  ```
   --
   --  **Network Operations:**
   --  ```ada
   --  Network_Buffer : Stream_Element_Array_Access := Receive_Data (Socket);
   --  Set_Data (Chunk, Network_Buffer);  -- Transfer ownership
   --  ```
   procedure Set_Data
     (Chunk : in out Chunk_Type; Data : in out Stream_Element_Array_Access)
   with Pre => Data /= null, Post => Data = null;

   function Data_Size (Chunk : Chunk_Type) return Natural
   with Post => Data_Size'Result >= 0, Inline;

   procedure Set_Data_Size (Chunk : in out Chunk_Type; Size : Natural)
   with Post => Data_Size (Chunk) = Size;

   --  Compression info
   function Is_Compressed (Chunk : Chunk_Type) return Boolean
   with Inline;

   procedure Set_Compressed (Chunk : in out Chunk_Type; Compressed : Boolean)
   with Post => Is_Compressed (Chunk) = Compressed;

   function Original_Size (Chunk : Chunk_Type) return Natural
   with
     Post =>
       (if Is_Compressed (Chunk) then Original_Size'Result > 0
        else Original_Size'Result = 0),
     Inline;

   procedure Set_Original_Size (Chunk : in out Chunk_Type; Size : Natural)
   with Post => Original_Size (Chunk) = Size;

   --  Processing metrics
   function Retry_Count (Chunk : Chunk_Type) return Natural
   with Post => Retry_Count'Result >= 0, Inline;

   procedure Increment_Retry_Count (Chunk : in out Chunk_Type)
   with
     Pre => Retry_Count (Chunk) < Natural'Last,
     Post => Retry_Count (Chunk) = Retry_Count (Chunk)'Old + 1;

   --  Validation
   function Is_Valid (Chunk : Chunk_Type) return Boolean
   with
     Post =>
       Is_Valid'Result
       = (Chunk.Number >= 0
          and then (if Chunk.Data /= null then Chunk.Data_Size > 0)
          and then (if Chunk.Data = null then Chunk.Data_Size = 0)
          and then (if Chunk.Is_Compressed and Chunk.Data /= null
                    then Chunk.Original_Size > Chunk.Data_Size)
          and then Chunk.Retry_Count >= 0);

   --  State transition validation
   function Is_Valid_Transition (From, To : Chunk_State) return Boolean
   with
     Post =>
       Is_Valid_Transition'Result
       = (case From is
            when Created => To in Reading | Read,
            when Reading => To in Read | Created,  -- Can retry
            when Read => To in Processing | Writing,
            when Processing => To in Processed | Read,  -- Can retry
            when Processed => To in Writing,
            when Writing => To in Written | Processed,  -- Can retry
            when Written => False);  -- Terminal state

   --  Reset chunk for reuse (object pooling support)
   procedure Reset (Chunk : in out Chunk_Type)
   with
     Post =>
       State (Chunk) = Created
       and Data (Chunk) = null
       and Data_Size (Chunk) = 0;

private

   type Chunk_Type is tagged record
      --  Identity
      Number : Natural := 0;

      --  State
      State : Chunk_State := Created;

      --  Data
      Data           : Stream_Element_Array_Access := null;
      Data_Size      : Natural := 0;
      Allocated_Size :
        Pipelib.Core.Domain.Value_Objects.Chunk_Size.Chunk_Size_Type;

      --  Compression info
      Is_Compressed : Boolean := False;
      Original_Size : Natural := 0;

      --  Metrics
      Retry_Count : Natural := 0;
   end record;

   function Number (Chunk : Chunk_Type) return Natural
   is (Chunk.Number);
   function State (Chunk : Chunk_Type) return Chunk_State
   is (Chunk.State);
   function Data (Chunk : Chunk_Type) return Stream_Element_Array_Access
   is (Chunk.Data);
   function Data_Size (Chunk : Chunk_Type) return Natural
   is (Chunk.Data_Size);
   function Is_Compressed (Chunk : Chunk_Type) return Boolean
   is (Chunk.Is_Compressed);
   function Original_Size (Chunk : Chunk_Type) return Natural
   is (Chunk.Original_Size);
   function Retry_Count (Chunk : Chunk_Type) return Natural
   is (Chunk.Retry_Count);

end Pipelib.Core.Domain.Entities.Chunk;
