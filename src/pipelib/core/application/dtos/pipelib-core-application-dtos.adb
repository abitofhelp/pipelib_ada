--  =============================================================================
--  Pipelib.Core.Application.DTOs - Implementation
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--  =============================================================================

pragma Ada_2022;

package body Pipelib.Core.Application.DTOs is

   function Create_Process_Request
     (Data_Address    : System.Address;
      Data_Size       : Storage_Count;
      File_Position   : File_Position_Type;
      Sequence_Number : Sequence_Number_Type := 0;
      Priority        : Processing_Priority := Normal;
      Is_Final        : Boolean := False) return Process_Chunk_Request is
   begin
      return
        (Data_Address    => Data_Address,
         Data_Size       => Buffer_Size_Type (Data_Size),
         File_Position   => File_Position,
         Sequence_Number => Sequence_Number,
         Priority        => Priority,
         Is_Final_Chunk  => Is_Final);
   end Create_Process_Request;

   function Create_Success_Response
     (Sequence        : Sequence_Number_Type;
      Bytes_Processed : Storage_Count;
      Processing_Time : Processing_Time_Ms_Type) return Process_Chunk_Response is
   begin
      return
        (Success            => True,
         Actual_Sequence    => Sequence,
         Bytes_Processed    => SI_Bytes_Type (Bytes_Processed),
         Processing_Time_Ms => Processing_Time,
         Error_Message      => Null_Unbounded_String);
   end Create_Success_Response;

   function Create_Error_Response
     (Error_Message : String) return Process_Chunk_Response is
   begin
      return
        (Success            => False,
         Actual_Sequence    => 0,
         Bytes_Processed    => 0,
         Processing_Time_Ms => 0,
         Error_Message      => To_Unbounded_String (Error_Message));
   end Create_Error_Response;

   function Is_Valid_Configuration
     (Config : Pipeline_Configuration) return Boolean is
   begin
      -- All numeric fields are already validated by their type constraints:
      -- Worker_Count is Positive range 1..64 (always valid)
      -- Chunk_Size_Bytes is Positive (always > 0)
      -- Max_Queue_Depth is Positive (always > 0)
      -- Only need to check the unbounded string
      return Length (Config.Output_File_Path) > 0;
   end Is_Valid_Configuration;

   function Status_Description (Status : Processing_Status) return String is
   begin
      case Status is
         when Not_Started =>
            return "Pipeline not yet started";

         when Starting =>
            return "Pipeline starting up";

         when Running =>
            return "Pipeline running normally";

         when Completing =>
            return "Pipeline completing final tasks";

         when Completed =>
            return "Pipeline completed successfully";

         when Failed =>
            return "Pipeline failed with error";

         when Cancelled =>
            return "Pipeline cancelled by request";
      end case;
   end Status_Description;

end Pipelib.Core.Application.DTOs;
