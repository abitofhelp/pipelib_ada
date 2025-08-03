--  =============================================================================
--  Test_Random_Write_File - Random Write File Integration Tests Specification
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Comprehensive integration tests for the Random Write File infrastructure
--    component using the abohlib testing framework with real file I/O.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Random_Write_File is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   with Post => Run_All_Tests'Result.Is_Ok or Run_All_Tests'Result.Is_Err;
   --  Execute all Random Write File tests and return statistics

private

   --  Basic file operations
   function Test_Create_And_Open return Void_Result.Result
   with Post => Test_Create_And_Open'Result.Is_Ok or
                Test_Create_And_Open'Result.Is_Err;
   --  Test creating and opening a file for random write

   function Test_Create_With_Preallocate return Void_Result.Result
   with Post => Test_Create_With_Preallocate'Result.Is_Ok or
                Test_Create_With_Preallocate'Result.Is_Err;
   --  Test creating a file with pre-allocated size

   function Test_Create_Without_Temp return Void_Result.Result
   with Post => Test_Create_Without_Temp'Result.Is_Ok or
                Test_Create_Without_Temp'Result.Is_Err;
   --  Test creating a file without using temp file

   --  Write operations
   function Test_Write_Single_Chunk return Void_Result.Result
   with Post => Test_Write_Single_Chunk'Result.Is_Ok or
                Test_Write_Single_Chunk'Result.Is_Err;
   --  Test writing a single chunk

   function Test_Write_Multiple_Chunks return Void_Result.Result
   with Post => Test_Write_Multiple_Chunks'Result.Is_Ok or
                Test_Write_Multiple_Chunks'Result.Is_Err;
   --  Test writing multiple chunks sequentially

   function Test_Write_Random_Positions return Void_Result.Result
   with Post => Test_Write_Random_Positions'Result.Is_Ok or
                Test_Write_Random_Positions'Result.Is_Err;
   --  Test writing chunks at random positions

   function Test_Write_Overlapping_Chunks return Void_Result.Result
   with Post => Test_Write_Overlapping_Chunks'Result.Is_Ok or
                Test_Write_Overlapping_Chunks'Result.Is_Err;
   --  Test writing chunks that overlap

   function Test_Write_Beyond_EOF return Void_Result.Result
   with Post => Test_Write_Beyond_EOF'Result.Is_Ok or
                Test_Write_Beyond_EOF'Result.Is_Err;
   --  Test writing beyond current end of file

   --  Chunk sequence operations
   function Test_Write_By_Sequence_Number return Void_Result.Result
   with Post => Test_Write_By_Sequence_Number'Result.Is_Ok or
                Test_Write_By_Sequence_Number'Result.Is_Err;
   --  Test writing chunks using their sequence numbers

   function Test_Write_Out_Of_Order return Void_Result.Result
   with Post => Test_Write_Out_Of_Order'Result.Is_Ok or
                Test_Write_Out_Of_Order'Result.Is_Err;
   --  Test writing chunks out of sequence order

   --  Commit and rollback operations
   function Test_Commit_Success return Void_Result.Result
   with Post => Test_Commit_Success'Result.Is_Ok or
                Test_Commit_Success'Result.Is_Err;
   --  Test successful commit of temp file to final

   function Test_Rollback_Success return Void_Result.Result
   with Post => Test_Rollback_Success'Result.Is_Ok or
                Test_Rollback_Success'Result.Is_Err;
   --  Test successful rollback of temp file

   function Test_Commit_After_Rollback return Void_Result.Result
   with Post => Test_Commit_After_Rollback'Result.Is_Ok or
                Test_Commit_After_Rollback'Result.Is_Err;
   --  Test commit after rollback (should fail)

   --  File management
   function Test_Flush_Operations return Void_Result.Result
   with Post => Test_Flush_Operations'Result.Is_Ok or
                Test_Flush_Operations'Result.Is_Err;
   --  Test flushing buffers to disk

   function Test_Preallocate_Space return Void_Result.Result
   with Post => Test_Preallocate_Space'Result.Is_Ok or
                Test_Preallocate_Space'Result.Is_Err;
   --  Test pre-allocating file space

   function Test_File_Size_Tracking return Void_Result.Result
   with Post => Test_File_Size_Tracking'Result.Is_Ok or
                Test_File_Size_Tracking'Result.Is_Err;
   --  Test accurate file size tracking

   --  Error handling
   function Test_Write_To_Closed_File return Void_Result.Result
   with Post => Test_Write_To_Closed_File'Result.Is_Ok or
                Test_Write_To_Closed_File'Result.Is_Err;
   --  Test writing to a closed file

   function Test_Invalid_Positions return Void_Result.Result
   with Post => Test_Invalid_Positions'Result.Is_Ok or
                Test_Invalid_Positions'Result.Is_Err;
   --  Test writing at invalid positions

   function Test_Disk_Full_Handling return Void_Result.Result
   with Post => Test_Disk_Full_Handling'Result.Is_Ok or
                Test_Disk_Full_Handling'Result.Is_Err;
   --  Test behavior when disk is full

   function Test_Permission_Errors return Void_Result.Result
   with Post => Test_Permission_Errors'Result.Is_Ok or
                Test_Permission_Errors'Result.Is_Err;
   --  Test handling of permission errors

   --  Concurrent access
   function Test_Protected_Concurrent_Writes return Void_Result.Result
   with Post => Test_Protected_Concurrent_Writes'Result.Is_Ok or
                Test_Protected_Concurrent_Writes'Result.Is_Err;
   --  Test thread-safe concurrent writes

   function Test_Protected_Write_Order return Void_Result.Result
   with Post => Test_Protected_Write_Order'Result.Is_Ok or
                Test_Protected_Write_Order'Result.Is_Err;
   --  Test write ordering with protected type

   function Test_Protected_Size_Query return Void_Result.Result
   with Post => Test_Protected_Size_Query'Result.Is_Ok or
                Test_Protected_Size_Query'Result.Is_Err;
   --  Test concurrent size queries

   --  Resource management
   function Test_Finalization_Cleanup return Void_Result.Result
   with Post => Test_Finalization_Cleanup'Result.Is_Ok or
                Test_Finalization_Cleanup'Result.Is_Err;
   --  Test automatic cleanup on finalization

   function Test_Destroy_Procedure return Void_Result.Result
   with Post => Test_Destroy_Procedure'Result.Is_Ok or
                Test_Destroy_Procedure'Result.Is_Err;
   --  Test explicit destruction

   function Test_Multiple_File_Handles return Void_Result.Result
   with Post => Test_Multiple_File_Handles'Result.Is_Ok or
                Test_Multiple_File_Handles'Result.Is_Err;
   --  Test managing multiple file handles

   --  Large file operations
   function Test_Large_File_Writing return Void_Result.Result
   with Post => Test_Large_File_Writing'Result.Is_Ok or
                Test_Large_File_Writing'Result.Is_Err;
   --  Test writing large files (> 2GB)

   function Test_Sparse_File_Creation return Void_Result.Result
   with Post => Test_Sparse_File_Creation'Result.Is_Ok or
                Test_Sparse_File_Creation'Result.Is_Err;
   --  Test creating sparse files with gaps

end Test_Random_Write_File;
