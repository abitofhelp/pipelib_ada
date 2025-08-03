--  =============================================================================
--  Test_Memory_Mapped_Chunk_Adapter - Memory Mapped Chunk Adapter Integration Tests Specification
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Comprehensive integration tests for the Memory Mapped Chunk Adapter using the
--    abohlib testing framework with mocks and contracts.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Memory_Mapped_Chunk_Adapter is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   with Post => Run_All_Tests'Result.Is_Ok or Run_All_Tests'Result.Is_Err;
   --  Execute all Memory Mapped Chunk Adapter tests and return statistics

private

   --  Zero-copy operations tests
   function Test_Create_Chunks_Zero_Copy return Void_Result.Result
   with Post => Test_Create_Chunks_Zero_Copy'Result.Is_Ok or
                Test_Create_Chunks_Zero_Copy'Result.Is_Err;
   --  Verify zero-copy chunk creation from memory-mapped files

   function Test_Single_Chunk_Creation return Void_Result.Result
   with Post => Test_Single_Chunk_Creation'Result.Is_Ok or
                Test_Single_Chunk_Creation'Result.Is_Err;
   --  Test creation of single chunks with various parameters

   function Test_Large_File_Chunking return Void_Result.Result
   with Post => Test_Large_File_Chunking'Result.Is_Ok or
                Test_Large_File_Chunking'Result.Is_Err;
   --  Test chunking of large files with memory constraints

   --  Configuration and optimization tests
   function Test_Chunk_Config_Variations return Void_Result.Result
   with Post => Test_Chunk_Config_Variations'Result.Is_Ok or
                Test_Chunk_Config_Variations'Result.Is_Err;
   --  Test different chunk configurations and settings

   function Test_Optimal_Chunk_Size_Calculation return Void_Result.Result
   with Post => Test_Optimal_Chunk_Size_Calculation'Result.Is_Ok or
                Test_Optimal_Chunk_Size_Calculation'Result.Is_Err;
   --  Test adaptive chunk size calculation algorithms

   function Test_Memory_Mapping_Decision return Void_Result.Result
   with Post => Test_Memory_Mapping_Decision'Result.Is_Ok or
                Test_Memory_Mapping_Decision'Result.Is_Err;
   --  Test logic for when to use memory mapping vs traditional I/O

   --  Error handling and edge cases
   function Test_Invalid_Memory_Map_Handling return Void_Result.Result
   with Post => Test_Invalid_Memory_Map_Handling'Result.Is_Ok or
                Test_Invalid_Memory_Map_Handling'Result.Is_Err;
   --  Test behavior with invalid or unmapped memory maps

   function Test_Memory_Access_Violations return Void_Result.Result
   with Post => Test_Memory_Access_Violations'Result.Is_Ok or
                Test_Memory_Access_Violations'Result.Is_Err;
   --  Test handling of out-of-bounds memory access attempts

   function Test_Resource_Cleanup return Void_Result.Result
   with Post => Test_Resource_Cleanup'Result.Is_Ok or
                Test_Resource_Cleanup'Result.Is_Err;
   --  Test proper cleanup of memory mappings and resources

   --  Performance and concurrency tests
   function Test_Concurrent_Chunk_Creation return Void_Result.Result
   with Post => Test_Concurrent_Chunk_Creation'Result.Is_Ok or
                Test_Concurrent_Chunk_Creation'Result.Is_Err;
   --  Test thread-safe chunk creation from shared memory maps

   function Test_Memory_Pressure_Scenarios return Void_Result.Result
   with Post => Test_Memory_Pressure_Scenarios'Result.Is_Ok or
                Test_Memory_Pressure_Scenarios'Result.Is_Err;
   --  Test behavior under memory pressure conditions

   --  Integration with memory-mapped file interface
   function Test_Memory_Map_Interface_Integration return Void_Result.Result
   with Post => Test_Memory_Map_Interface_Integration'Result.Is_Ok or
                Test_Memory_Map_Interface_Integration'Result.Is_Err;
   --  Test integration with different memory map implementations

   function Test_Checksum_Calculation return Void_Result.Result
   with Post => Test_Checksum_Calculation'Result.Is_Ok or
                Test_Checksum_Calculation'Result.Is_Err;
   --  Test checksum calculation for chunks with zero-copy access

end Test_Memory_Mapped_Chunk_Adapter;
