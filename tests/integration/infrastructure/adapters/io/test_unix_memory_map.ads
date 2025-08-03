--  =============================================================================
--  Test_Unix_Memory_Map - Unix Memory Map Adapter Integration Tests Specification
--  =============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  Purpose:
--    Comprehensive integration tests for the Unix Memory Map adapter
--    testing real Unix mmap system calls and domain interface compliance.
--  =============================================================================

pragma Ada_2022;

with Abohlib.Infrastructure.Testing.Test_Framework;

package Test_Unix_Memory_Map is

   use Abohlib.Infrastructure.Testing.Test_Framework;

   function Run_All_Tests
     (Output : access Test_Output_Port'Class) return Test_Stats_Result.Result
   with Post => Run_All_Tests'Result.Is_Ok or Run_All_Tests'Result.Is_Err;
   --  Execute all Unix Memory Map tests and return statistics

private

   --  Unix-specific mmap tests
   function Test_Mmap_System_Call return Void_Result.Result
   with Post => Test_Mmap_System_Call'Result.Is_Ok or
                Test_Mmap_System_Call'Result.Is_Err;
   --  Test direct mmap system call functionality

   function Test_Mmap_Protection_Flags return Void_Result.Result
   with Post => Test_Mmap_Protection_Flags'Result.Is_Ok or
                Test_Mmap_Protection_Flags'Result.Is_Err;
   --  Test PROT_READ, PROT_WRITE flags

   function Test_Mmap_Sharing_Flags return Void_Result.Result
   with Post => Test_Mmap_Sharing_Flags'Result.Is_Ok or
                Test_Mmap_Sharing_Flags'Result.Is_Err;
   --  Test MAP_SHARED vs MAP_PRIVATE behavior

   function Test_Mmap_Page_Alignment return Void_Result.Result
   with Post => Test_Mmap_Page_Alignment'Result.Is_Ok or
                Test_Mmap_Page_Alignment'Result.Is_Err;
   --  Test page boundary alignment requirements

   --  File descriptor management
   function Test_File_Descriptor_Lifecycle return Void_Result.Result
   with Post => Test_File_Descriptor_Lifecycle'Result.Is_Ok or
                Test_File_Descriptor_Lifecycle'Result.Is_Err;
   --  Test proper opening and closing of file descriptors

   function Test_Invalid_File_Descriptor return Void_Result.Result
   with Post => Test_Invalid_File_Descriptor'Result.Is_Ok or
                Test_Invalid_File_Descriptor'Result.Is_Err;
   --  Test handling of invalid file descriptors

   function Test_File_Descriptor_Limits return Void_Result.Result
   with Post => Test_File_Descriptor_Limits'Result.Is_Ok or
                Test_File_Descriptor_Limits'Result.Is_Err;
   --  Test behavior at file descriptor limits

   --  Memory synchronization
   function Test_Msync_Synchronous return Void_Result.Result
   with Post => Test_Msync_Synchronous'Result.Is_Ok or
                Test_Msync_Synchronous'Result.Is_Err;
   --  Test synchronous msync operations

   function Test_Msync_Asynchronous return Void_Result.Result
   with Post => Test_Msync_Asynchronous'Result.Is_Ok or
                Test_Msync_Asynchronous'Result.Is_Err;
   --  Test asynchronous msync operations

   function Test_Msync_Invalidate return Void_Result.Result
   with Post => Test_Msync_Invalidate'Result.Is_Ok or
                Test_Msync_Invalidate'Result.Is_Err;
   --  Test msync with invalidate flag

   --  Memory advisement
   function Test_Madvise_Sequential return Void_Result.Result
   with Post => Test_Madvise_Sequential'Result.Is_Ok or
                Test_Madvise_Sequential'Result.Is_Err;
   --  Test MADV_SEQUENTIAL behavior

   function Test_Madvise_Random return Void_Result.Result
   with Post => Test_Madvise_Random'Result.Is_Ok or
                Test_Madvise_Random'Result.Is_Err;
   --  Test MADV_RANDOM behavior

   function Test_Madvise_Willneed return Void_Result.Result
   with Post => Test_Madvise_Willneed'Result.Is_Ok or
                Test_Madvise_Willneed'Result.Is_Err;
   --  Test MADV_WILLNEED prefetching

   function Test_Madvise_Dontneed return Void_Result.Result
   with Post => Test_Madvise_Dontneed'Result.Is_Ok or
                Test_Madvise_Dontneed'Result.Is_Err;
   --  Test MADV_DONTNEED page release

   --  Domain interface compliance
   function Test_Interface_Implementation return Void_Result.Result
   with Post => Test_Interface_Implementation'Result.Is_Ok or
                Test_Interface_Implementation'Result.Is_Err;
   --  Test full domain interface implementation

   function Test_Interface_Contracts return Void_Result.Result
   with Post => Test_Interface_Contracts'Result.Is_Ok or
                Test_Interface_Contracts'Result.Is_Err;
   --  Test preconditions and postconditions

   function Test_Interface_Error_Propagation return Void_Result.Result
   with Post => Test_Interface_Error_Propagation'Result.Is_Ok or
                Test_Interface_Error_Propagation'Result.Is_Err;
   --  Test Result type error handling

   --  Unix-specific error conditions
   function Test_Errno_Handling return Void_Result.Result
   with Post => Test_Errno_Handling'Result.Is_Ok or
                Test_Errno_Handling'Result.Is_Err;
   --  Test proper errno to Result conversion

   function Test_Signal_Handling return Void_Result.Result
   with Post => Test_Signal_Handling'Result.Is_Ok or
                Test_Signal_Handling'Result.Is_Err;
   --  Test SIGBUS and SIGSEGV handling

   function Test_Permission_Errors return Void_Result.Result
   with Post => Test_Permission_Errors'Result.Is_Ok or
                Test_Permission_Errors'Result.Is_Err;
   --  Test EACCES and EPERM handling

   --  Resource limits
   function Test_Memory_Lock_Limits return Void_Result.Result
   with Post => Test_Memory_Lock_Limits'Result.Is_Ok or
                Test_Memory_Lock_Limits'Result.Is_Err;
   --  Test mlockall/munlockall limits

   function Test_Virtual_Memory_Limits return Void_Result.Result
   with Post => Test_Virtual_Memory_Limits'Result.Is_Ok or
                Test_Virtual_Memory_Limits'Result.Is_Err;
   --  Test system virtual memory limits

   function Test_Address_Space_Exhaustion return Void_Result.Result
   with Post => Test_Address_Space_Exhaustion'Result.Is_Ok or
                Test_Address_Space_Exhaustion'Result.Is_Err;
   --  Test behavior when address space is exhausted

   --  Special file types
   function Test_Device_File_Mapping return Void_Result.Result
   with Post => Test_Device_File_Mapping'Result.Is_Ok or
                Test_Device_File_Mapping'Result.Is_Err;
   --  Test mapping device files

   function Test_Sparse_File_Mapping return Void_Result.Result
   with Post => Test_Sparse_File_Mapping'Result.Is_Ok or
                Test_Sparse_File_Mapping'Result.Is_Err;
   --  Test mapping sparse files with holes

   function Test_Huge_Page_Support return Void_Result.Result
   with Post => Test_Huge_Page_Support'Result.Is_Ok or
                Test_Huge_Page_Support'Result.Is_Err;
   --  Test transparent huge page support

   --  Concurrent access
   function Test_Fork_Inheritance return Void_Result.Result
   with Post => Test_Fork_Inheritance'Result.Is_Ok or
                Test_Fork_Inheritance'Result.Is_Err;
   --  Test mapping inheritance across fork

   function Test_Thread_Safety return Void_Result.Result
   with Post => Test_Thread_Safety'Result.Is_Ok or
                Test_Thread_Safety'Result.Is_Err;
   --  Test thread-safe access to mappings

   function Test_Shared_Memory_Consistency return Void_Result.Result
   with Post => Test_Shared_Memory_Consistency'Result.Is_Ok or
                Test_Shared_Memory_Consistency'Result.Is_Err;
   --  Test MAP_SHARED consistency across processes

end Test_Unix_Memory_Map;
