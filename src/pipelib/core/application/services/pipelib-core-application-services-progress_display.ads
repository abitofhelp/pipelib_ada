--  =============================================================================
--  Pipelib.Core.Application.Services.Progress_Display - Progress Display Service
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: MIT
--
--  ## Progress Display Application Service
--
--  Application layer service responsible for presenting progress information
--  to users. This service takes domain progress state and formats it for
--  console display using ANSI escape codes for real-time updates.
--
--  ### Architecture Compliance
--
--  **Separation of Concerns**: This service handles presentation logic while
--  the domain Progress_Tracker focuses purely on business state tracking.
--
--  **Dependency Direction**: Depends only on domain layer Progress_State,
--  maintaining proper Clean Architecture boundaries.
--
--  **Single Responsibility**: Focused solely on progress presentation without
--  any business logic or state management concerns.
--
--  ### Display Features
--
--  **Real-Time Updates**: Uses ANSI escape codes for in-place console updates
--  * Saves/restores cursor position for non-scrolling display
--  * Clears lines to handle varying text lengths
--  * Provides smooth visual progress indication
--
--  **Visual Formatting**: Professional progress display with:
--  * Right-aligned numeric values for clean presentation
--  * Color-coded completion indicators using ANSI colors
--  * Consistent spacing and layout across all progress stages
--  * Hidden cursor during updates to avoid visual artifacts
--
--  **Stage Tracking**: Displays all three pipeline stages:
--  * Read stage with chunk count and completion status
--  * Processing stage with transformation progress
--  * Write stage with output progress and final completion
--
--  ### Usage Pattern
--
--  ```ada
--  -- Application orchestration
--  procedure Display_Pipeline_Progress(Tracker : Progress_Tracker_Access) is
--     Display_Service : Progress_Display_Service.Progress_Display_Type;
--  begin
--     -- Initialize display
--     Display_Service.Initialize_Display;
--
--     -- Update loop
--     while not Tracker.Is_All_Complete loop
--        delay 0.1;  -- Update every 100ms
--        Display_Service.Display_Progress(Tracker.Get_Progress_State);
--     end loop;
--
--     -- Final display and cleanup
--     Display_Service.Display_Progress(Tracker.Get_Progress_State);
--     Display_Service.Finalize_Display;
--  end Display_Pipeline_Progress;
--  ```
--
--  ### Thread Safety
--
--  This service is designed for single-threaded access from a dedicated
--  display task. Multiple concurrent calls may result in output conflicts.
--
--  ### Performance Considerations
--
--  * Minimal overhead - only formatting and I/O operations
--  * No memory allocation during normal operation
--  * Fast ANSI escape sequence generation
--  * Buffered output for optimal terminal performance
--
--  =============================================================================

pragma Ada_2022;

with Pipelib.Core.Domain.Services.Progress_Tracker;

package Pipelib.Core.Application.Services.Progress_Display is

   --  ## Progress Display Service Type
   --
   --  Application service that handles all presentation aspects of progress
   --  reporting. Maintains display state (cursor position, formatting) while
   --  consuming domain progress state for actual data.
   type Progress_Display_Type is tagged limited private;

   --  ## Initialize Progress Display
   --
   --  Sets up the console display area and saves initial cursor position.
   --  Must be called before any display operations to ensure proper formatting.
   --
   --  ### Console Setup
   --  * Saves current cursor position for restoration
   --  * Prepares ANSI escape sequences for in-place updates
   --  * Initializes internal display state tracking
   --
   --  ### Error Handling
   --  Safe to call multiple times - subsequent calls are ignored.
   procedure Initialize_Display (Display : in out Progress_Display_Type);

   --  ## Display Current Progress
   --
   --  Renders the current progress state to the console using in-place updates.
   --  Creates a clean, professional progress display that updates existing lines.
   --
   --  ### Parameters
   --  * `Display` - The display service instance (maintains state)
   --  * `State` - Current progress state from domain tracker
   --
   --  ### Display Format
   --  ```
   --  Read:        42 [OK]
   --  Processed:   38
   --  Written:     35
   --  ```
   --
   --  ### Visual Features
   --  * Right-aligned numbers for clean presentation
   --  * Green [OK] indicators for completed stages
   --  * In-place updates without scrolling
   --  * Consistent spacing and formatting
   procedure Display_Progress
     (Display : in out Progress_Display_Type;
      State   : Pipelib.Core.Domain.Services.Progress_Tracker.Progress_State);

   --  ## Finalize Progress Display
   --
   --  Completes the progress display and moves cursor to next line.
   --  Should be called when progress tracking is complete.
   --
   --  ### Cleanup Operations
   --  * Shows cursor (if hidden during updates)
   --  * Moves to next line for subsequent output
   --  * Ensures terminal is in clean state
   --
   --  ### Usage
   --  Always call after final progress display to ensure clean terminal state.
   procedure Finalize_Display (Display : in out Progress_Display_Type);

private

   type Progress_Display_Type is tagged limited record
      Is_Initialized : Boolean := False;
      First_Display  : Boolean := True;
   end record;

end Pipelib.Core.Application.Services.Progress_Display;
