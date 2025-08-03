--  =============================================================================
--  Test_Memory_Mapped_File - Memory Mapped File Integration Tests Specification
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Comprehensive integration tests for the Memory Mapped File infrastructure
--    component using the abohlib testing framework with real I/O operations.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Memory_Mapped_File is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   with Post => Run_All_Tests'Result.Is_Ok or Run_All_Tests'Result.Is_Err;
   --  Execute all Memory Mapped File tests and return statistics

private

   --  Basic file mapping operations
   function Test_Map_Small_File return Void_Result.Result
   with Post => Test_Map_Small_File'Result.Is_Ok or
                Test_Map_Small_File'Result.Is_Err;
   --  Test mapping a small file (< 1MB) into memory

   function Test_Map_Large_File return Void_Result.Result
   with Post => Test_Map_Large_File'Result.Is_Ok or
                Test_Map_Large_File'Result.Is_Err;
   --  Test mapping a large file (> 100MB) into memory

   function Test_Map_Empty_File return Void_Result.Result
   with Post => Test_Map_Empty_File'Result.Is_Ok or
                Test_Map_Empty_File'Result.Is_Err;
   --  Test mapping an empty file

   function Test_Map_Nonexistent_File return Void_Result.Result
   with Post => Test_Map_Nonexistent_File'Result.Is_Ok or
                Test_Map_Nonexistent_File'Result.Is_Err;
   --  Test mapping a file that doesn't exist

   --  Read/Write operations
   function Test_Read_Only_Mapping return Void_Result.Result
   with Post => Test_Read_Only_Mapping'Result.Is_Ok or
                Test_Read_Only_Mapping'Result.Is_Err;
   --  Test read-only memory mapping

   function Test_Read_Write_Mapping return Void_Result.Result
   with Post => Test_Read_Write_Mapping'Result.Is_Ok or
                Test_Read_Write_Mapping'Result.Is_Err;
   --  Test read-write memory mapping with modifications

   function Test_Multiple_Mappings return Void_Result.Result
   with Post => Test_Multiple_Mappings'Result.Is_Ok or
                Test_Multiple_Mappings'Result.Is_Err;
   --  Test multiple mappings of the same file

   --  Subview operations
   function Test_Create_Subview_Valid return Void_Result.Result
   with Post => Test_Create_Subview_Valid'Result.Is_Ok or
                Test_Create_Subview_Valid'Result.Is_Err;
   --  Test creating valid subviews of mapped memory

   function Test_Create_Subview_Boundary return Void_Result.Result
   with Post => Test_Create_Subview_Boundary'Result.Is_Ok or
                Test_Create_Subview_Boundary'Result.Is_Err;
   --  Test subview creation at file boundaries

   function Test_Create_Subview_Invalid return Void_Result.Result
   with Post => Test_Create_Subview_Invalid'Result.Is_Ok or
                Test_Create_Subview_Invalid'Result.Is_Err;
   --  Test invalid subview parameters

   --  Synchronization operations
   function Test_Sync_Changes return Void_Result.Result
   with Post => Test_Sync_Changes'Result.Is_Ok or
                Test_Sync_Changes'Result.Is_Err;
   --  Test syncing changes to disk

   function Test_Sync_Read_Only return Void_Result.Result
   with Post => Test_Sync_Read_Only'Result.Is_Ok or
                Test_Sync_Read_Only'Result.Is_Err;
   --  Test sync on read-only mappings

   --  Access pattern advisement
   function Test_Advise_Sequential return Void_Result.Result
   with Post => Test_Advise_Sequential'Result.Is_Ok or
                Test_Advise_Sequential'Result.Is_Err;
   --  Test sequential access pattern advisement

   function Test_Advise_Random return Void_Result.Result
   with Post => Test_Advise_Random'Result.Is_Ok or
                Test_Advise_Random'Result.Is_Err;
   --  Test random access pattern advisement

   function Test_Advise_Will_Need return Void_Result.Result
   with Post => Test_Advise_Will_Need'Result.Is_Ok or
                Test_Advise_Will_Need'Result.Is_Err;
   --  Test will-need pattern advisement

   function Test_Advise_Dont_Need return Void_Result.Result
   with Post => Test_Advise_Dont_Need'Result.Is_Ok or
                Test_Advise_Dont_Need'Result.Is_Err;
   --  Test don't-need pattern advisement

   --  Resource management
   function Test_Unmap_Cleanup return Void_Result.Result
   with Post => Test_Unmap_Cleanup'Result.Is_Ok or
                Test_Unmap_Cleanup'Result.Is_Err;
   --  Test proper resource cleanup on unmap

   function Test_Finalization return Void_Result.Result
   with Post => Test_Finalization'Result.Is_Ok or
                Test_Finalization'Result.Is_Err;
   --  Test automatic cleanup on object destruction

   function Test_Double_Unmap return Void_Result.Result
   with Post => Test_Double_Unmap'Result.Is_Ok or
                Test_Double_Unmap'Result.Is_Err;
   --  Test unmapping an already unmapped file

   --  Platform capability tests
   function Test_Memory_Mapping_Available return Void_Result.Result
   with Post => Test_Memory_Mapping_Available'Result.Is_Ok or
                Test_Memory_Mapping_Available'Result.Is_Err;
   --  Test platform memory mapping availability

   function Test_Should_Use_Memory_Map return Void_Result.Result
   with Post => Test_Should_Use_Memory_Map'Result.Is_Ok or
                Test_Should_Use_Memory_Map'Result.Is_Err;
   --  Test decision logic for using memory mapping

   --  Error handling
   function Test_Map_Protected_File return Void_Result.Result
   with Post => Test_Map_Protected_File'Result.Is_Ok or
                Test_Map_Protected_File'Result.Is_Err;
   --  Test mapping files with restricted permissions

   function Test_Map_Directory return Void_Result.Result
   with Post => Test_Map_Directory'Result.Is_Ok or
                Test_Map_Directory'Result.Is_Err;
   --  Test attempting to map a directory

   function Test_Map_Special_File return Void_Result.Result
   with Post => Test_Map_Special_File'Result.Is_Ok or
                Test_Map_Special_File'Result.Is_Err;
   --  Test mapping special files (devices, pipes, etc.)

   --  Concurrency tests
   function Test_Concurrent_Reads return Void_Result.Result
   with Post => Test_Concurrent_Reads'Result.Is_Ok or
                Test_Concurrent_Reads'Result.Is_Err;
   --  Test concurrent read access to mapped memory

   function Test_Concurrent_Modifications return Void_Result.Result
   with Post => Test_Concurrent_Modifications'Result.Is_Ok or
                Test_Concurrent_Modifications'Result.Is_Err;
   --  Test concurrent write access with proper synchronization

end Test_Memory_Mapped_File;
