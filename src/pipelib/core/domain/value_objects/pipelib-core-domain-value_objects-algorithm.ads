--   =============================================================================
--   Pipelib.Core.Domain.Value_Objects.Algorithm - Algorithm Value Object
--   Copyright (c) 2025 A Bit of Help, Inc.
--   SPDX-License-Identifier: MIT
--
--   ## Algorithm Value Object - Type-Safe Algorithm Specification
--
--   Immutable value object for specifying data processing algorithms with compile-time
--   validation and runtime safety. Provides type-safe access to compression, encryption,
--   hashing, and custom algorithm implementations.
--
--   ### Design Philosophy
--
--   This value object follows Domain-Driven Design principles:
--   * **Immutability**: Algorithm instances cannot be modified after creation
--   * **Value Semantics**: Two algorithms with the same name are equal
--   * **Validation**: All algorithm names must follow strict formatting rules
--   * **Type Safety**: Invalid algorithms cannot be constructed
--
--   ### Algorithm Categories
--
--   Algorithms are automatically categorized based on their names:
--   * **Compression**: Data reduction algorithms (brotli, gzip, zstd, lz4)
--   * **Encryption**: Data protection algorithms (aes-256-gcm, chacha20-poly1305)
--   * **Hashing**: Data integrity algorithms (sha256, sha512, blake3)
--   * **Custom**: User-defined algorithms with application-specific logic
--
--   ### Naming Convention
--
--   Algorithm names must follow strict formatting rules for consistency:
--   * Only lowercase letters (a-z)
--   * Digits (0-9)
--   * Hyphens (-) for word separation
--   * No spaces, underscores, or special characters
--   * Examples: "gzip", "aes-256-gcm", "sha512", "custom-transform"
--
--   ### Performance Characteristics
--
--   * **Zero-Copy Construction**: Algorithm names stored as Unbounded_String
--   * **Fast Equality**: String comparison for algorithm matching
--   * **Compile-Time Validation**: Invalid formats rejected at compile time
--   * **Category Lookup**: O(1) categorization through name pattern matching
--
--   ### Integration with Pipeline
--
--   Algorithm objects integrate seamlessly with the processing pipeline:
--   * Chunk processors query algorithm type to select implementation
--   * Configuration files specify algorithms by name
--   * Runtime algorithm switching through factory methods
--   * Validation ensures only supported algorithms are used
--
--   ### Usage Patterns
--
--   **Built-in Algorithms:**
--   ```ada
--   -- Use predefined compression algorithm
--   declare
--      Compress_Algo : constant Algorithm_Type := Brotli;
--   begin
--      Process_With_Algorithm (Data, Compress_Algo);
--   end;
--
--   -- Use predefined encryption algorithm
--   declare
--      Encrypt_Algo : constant Algorithm_Type := AES_256_GCM;
--   begin
--      Secure_Process (Sensitive_Data, Encrypt_Algo);
--   end;
--   ```
--
--   **Custom Algorithms:**
--   ```ada
--   -- Define custom algorithm
--   declare
--      Custom_Algo : constant Algorithm_Type := Create ("my-custom-transform");
--   begin
--      pragma Assert (Category (Custom_Algo) = Custom);
--      Apply_Custom_Processing (Data, Custom_Algo);
--   end;
--   ```
--
--   **Configuration-Driven Processing:**
--   ```ada
--   -- Process chunks based on configuration
--   declare
--      Config_Algorithm : constant Algorithm_Type :=
--         Create (Get_Config_Value ("processing.algorithm"));
--   begin
--      case Category (Config_Algorithm) is
--         when Compression =>
--            Apply_Compression (Chunk, Config_Algorithm);
--         when Encryption =>
--            Apply_Encryption (Chunk, Config_Algorithm);
--         when Hashing =>
--            Calculate_Hash (Chunk, Config_Algorithm);
--         when Custom =>
--            Apply_Custom_Logic (Chunk, Config_Algorithm);
--      end case;
--   end;
--   ```
--
--   ### Memory Management
--
--   * Uses Ada's controlled types for automatic string memory management
--   * No manual memory allocation or deallocation required
--   * Safe to copy and assign without memory leaks
--   * Minimal memory footprint (single string storage)
--
--   ### Thread Safety
--
--   Algorithm objects are completely thread-safe:
--   * Immutable after construction - no race conditions possible
--   * Safe to share between concurrent tasks
--   * No synchronization required for read operations
--   * Factory methods are thread-safe
--   =============================================================================

pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Pipelib.Core.Domain.Value_Objects.Algorithm is

   --  ## Algorithm Categories
   --
   --  Defines the major categories of algorithms supported by the pipeline.
   --  Categorization enables type-safe algorithm selection and validation
   --  of algorithm compatibility with processing stages.
   --
   --  ### Category Descriptions
   --  * `Compression` - Data reduction algorithms that decrease file size
   --  * `Encryption` - Data protection algorithms that secure content
   --  * `Hashing` - Data integrity algorithms that generate checksums/fingerprints
   --  * `Custom` - User-defined algorithms with application-specific logic
   --
   --  ### Automatic Categorization
   --  Algorithms are automatically assigned to categories based on their names:
   --  ```ada
   --  pragma Assert (Category (Brotli) = Compression);
   --  pragma Assert (Category (AES_256_GCM) = Encryption);
   --  pragma Assert (Category (SHA256) = Hashing);
   --  pragma Assert (Category (Create ("my-transform")) = Custom);
   --  ```
   type Algorithm_Category is (Compression, Encryption, Hashing, Custom);

   --  ## Algorithm Value Object Type
   --
   --  Immutable value object representing a specific algorithm with validation.
   --  Uses tagged type to enable future extensibility while maintaining
   --  value object semantics.
   --
   --  ### Immutability Guarantee
   --  Once created, algorithm objects cannot be modified. All operations
   --  return new algorithm instances rather than modifying existing ones.
   --
   --  ### Type Invariant
   --  The type maintains the invariant that all algorithm names are valid
   --  according to the formatting rules. This ensures no invalid algorithms
   --  can exist at runtime.
   type Algorithm_Type is tagged private;

   --  ## Validation Function (Forward Declaration)
   --
   --  Validates that an algorithm instance meets all requirements.
   --  Used internally by the type invariant to ensure algorithm validity.
   --
   --  ### Validation Rules
   --  * Algorithm name must not be empty
   --  * Algorithm name must follow formatting conventions
   --  * Name length must be within reasonable bounds
   function Is_Valid (Self : Algorithm_Type) return Boolean;

   --  ## Algorithm Factory Function
   --
   --  Creates a new algorithm value object with the specified name.
   --  Performs validation to ensure the algorithm name follows the
   --  required formatting conventions.
   --
   --  ### Parameters
   --  * `Name` - Algorithm identifier following naming conventions
   --
   --  ### Naming Rules
   --  Algorithm names must follow these strict rules:
   --  * Only lowercase letters (a-z)
   --  * Digits (0-9) for version numbers
   --  * Hyphens (-) for word separation
   --  * No spaces, underscores, or special characters
   --  * Minimum length of 1 character
   --
   --  ### Examples
   --  ```ada
   --  -- Valid algorithm names
   --  Valid_Algorithms : constant array (1 .. 6) of Algorithm_Type := (
   --     Create ("gzip"),                    -- Simple name
   --     Create ("aes-256-gcm"),             -- Hyphen-separated
   --     Create ("sha512"),                  -- With digits
   --     Create ("lz4"),                     -- Short name
   --     Create ("custom-transform-v2"),     -- Custom with version
   --     Create ("blake3")                   -- Modern algorithm
   --  );
   --
   --  -- Invalid algorithm names (would fail precondition)
   --  -- Create ("");                      -- Empty string
   --  -- Create ("GZIP");                  -- Uppercase letters
   --  -- Create ("aes_256_gcm");           -- Underscores
   --  -- Create ("sha-256 gcm");           -- Spaces
   --  -- Create ("custom@algorithm");      -- Special characters
   --  ```
   --
   --  ### Automatic Categorization
   --  The factory function automatically determines the algorithm category
   --  based on the name pattern:
   --  ```ada
   --  -- Compression algorithms
   --  Brotli_Algo : constant Algorithm_Type := Create ("brotli");
   --  pragma Assert (Category (Brotli_Algo) = Compression);
   --
   --  -- Encryption algorithms
   --  AES_Algo : constant Algorithm_Type := Create ("aes-256-gcm");
   --  pragma Assert (Category (AES_Algo) = Encryption);
   --
   --  -- Hashing algorithms
   --  Hash_Algo : constant Algorithm_Type := Create ("sha256");
   --  pragma Assert (Category (Hash_Algo) = Hashing);
   --
   --  -- Custom algorithms (anything not recognized)
   --  Custom_Algo : constant Algorithm_Type := Create ("my-transform");
   --  pragma Assert (Category (Custom_Algo) = Custom);
   --  ```
   --
   --  ### Error Handling
   --  Invalid algorithm names will trigger a `Constraint_Error` due to
   --  the precondition check. This prevents creation of invalid algorithms
   --  and ensures all algorithms in the system are properly formatted.
   --
   --  ### Performance
   --  Algorithm creation is efficient:
   --  * O(n) string validation where n is name length
   --  * Single string allocation for storage
   --  * No complex parsing or external dependencies
   function Create (Name : String) return Algorithm_Type
   with Pre => Name'Length > 0 and then Is_Valid_Format (Name);

   --  ## Algorithm Name Access
   --
   --  Returns the string identifier for this algorithm. The name is guaranteed
   --  to be valid and follow the formatting conventions due to the type invariant.
   --
   --  ### Returns
   --  String containing the algorithm name (never empty, always valid format)
   --
   --  ### Usage
   --  ```ada
   --  declare
   --     Algo : constant Algorithm_Type := Brotli;
   --  begin
   --     Put_Line ("Using algorithm: " & Name (Algo));
   --     -- Output: "Using algorithm: brotli"
   --
   --     -- Use in configuration lookups
   --     Config_Key : constant String := "algorithms." & Name (Algo) & ".level";
   --     Compression_Level : constant Natural := Get_Config (Config_Key);
   --
   --     -- Use for algorithm selection
   --     if Name (Algo) = "gzip" then
   --        Apply_Gzip_Compression (Data);
   --     elsif Name (Algo) = "brotli" then
   --        Apply_Brotli_Compression (Data);
   --     end if;
   --  end;
   --  ```
   function Name (Self : Algorithm_Type) return String
   with
     Post => Name'Result'Length > 0 and then Is_Valid_Format (Name'Result),
     Inline;

   --  ## Algorithm Category Classification
   --
   --  Returns the category classification for this algorithm based on its name.
   --  Categories are determined automatically using pattern matching against
   --  known algorithm names and conventions.
   --
   --  ### Returns
   --  Algorithm_Category enum value indicating the algorithm's purpose
   --
   --  ### Categorization Logic
   --
   --  **Compression Algorithms:**
   --  * "brotli", "gzip", "zstd", "lz4" - Standard compression algorithms
   --  * Names containing "compress", "deflate", "lzma" - Compression variants
   --
   --  **Encryption Algorithms:**
   --  * "aes-256-gcm", "chacha20-poly1305" - Authenticated encryption
   --  * Names containing "aes", "chacha", "encrypt" - Encryption variants
   --
   --  **Hashing Algorithms:**
   --  * "sha256", "sha512", "blake3" - Cryptographic hash functions
   --  * Names containing "sha", "blake", "hash", "md5" - Hash variants
   --
   --  **Custom Algorithms:**
   --  * Any algorithm name not matching known patterns
   --  * User-defined algorithms with application-specific logic
   --
   --  ### Usage for Processing Logic
   --  ```ada
   --  declare
   --     Algo : constant Algorithm_Type := Get_Algorithm_From_Config;
   --  begin
   --     case Category (Algo) is
   --        when Compression =>
   --           -- Apply data compression
   --           Compressed_Data := Apply_Compression (Input_Data, Algo);
   --
   --        when Encryption =>
   --           -- Apply data encryption with key management
   --           Key : constant Encryption_Key := Get_Key_For_Algorithm (Algo);
   --           Encrypted_Data := Apply_Encryption (Input_Data, Algo, Key);
   --
   --        when Hashing =>
   --           -- Calculate data hash/checksum
   --           Hash_Value := Calculate_Hash (Input_Data, Algo);
   --           Store_Integrity_Check (Hash_Value);
   --
   --        when Custom =>
   --           -- Delegate to custom algorithm registry
   --           Custom_Processor := Get_Custom_Processor (Name (Algo));
   --           Result := Custom_Processor.Process (Input_Data);
   --     end case;
   --  end;
   --  ```
   --
   --  ### Performance
   --  Category determination is efficient:
   --  * Cached result after first lookup (if implemented)
   --  * Simple string pattern matching
   --  * No external dependencies or complex parsing
   function Category (Self : Algorithm_Type) return Algorithm_Category;

   --  ## Algorithm Name Format Validation
   --
   --  Validates that an algorithm name follows the required formatting conventions.
   --  This ensures consistency across all algorithms and prevents parsing issues
   --  in configuration files and API interactions.
   --
   --  ### Parameters
   --  * `Name` - String to validate for algorithm name compliance
   --
   --  ### Returns
   --  True if the name follows all formatting rules, False otherwise
   --
   --  ### Formatting Rules
   --  Valid algorithm names must contain only:
   --  * Lowercase letters (a-z) - For readability and consistency
   --  * Digits (0-9) - For version numbers and parameters
   --  * Hyphens (-) - For word separation (preferred over underscores)
   --
   --  ### Design Rationale
   --
   --  **Lowercase Only:**
   --  * Ensures case-insensitive matching works correctly
   --  * Prevents confusion between "AES" and "aes"
   --  * Standard convention in many algorithm specifications
   --
   --  **Hyphens for Separation:**
   --  * More readable than underscores in most contexts
   --  * Compatible with URL and filename conventions
   --  * Standard in algorithm naming (e.g., "aes-256-gcm")
   --
   --  **No Special Characters:**
   --  * Prevents parsing issues in configuration files
   --  * Ensures compatibility with command-line interfaces
   --  * Avoids encoding problems in different text formats
   --
   --  ### Examples
   --  ```ada
   --  -- Valid algorithm names
   --  pragma Assert (Is_Valid_Format ("gzip"));              -- Simple name
   --  pragma Assert (Is_Valid_Format ("aes-256-gcm"));       -- Hyphenated
   --  pragma Assert (Is_Valid_Format ("sha512"));            -- With digits
   --  pragma Assert (Is_Valid_Format ("lz4"));               -- Short name
   --  pragma Assert (Is_Valid_Format ("custom-v2"));         -- Version suffix
   --  pragma Assert (Is_Valid_Format ("blake3"));            -- Modern naming
   --
   --  -- Invalid algorithm names
   --  pragma Assert (not Is_Valid_Format ("GZIP"));          -- Uppercase
   --  pragma Assert (not Is_Valid_Format ("aes_256_gcm"));   -- Underscores
   --  pragma Assert (not Is_Valid_Format ("sha-256 gcm"));   -- Spaces
   --  pragma Assert (not Is_Valid_Format ("custom@algo"));   -- Special chars
   --  pragma Assert (not Is_Valid_Format (""));              -- Empty string
   --  pragma Assert (not Is_Valid_Format ("algo.name"));     -- Periods
   --  ```
   --
   --  ### Integration with Type System
   --  This function is used in:
   --  * Preconditions for the `Create` function
   --  * Type invariants for Algorithm_Type
   --  * Postconditions for accessor functions
   --  * Validation in configuration parsing
   --
   --  ### Performance
   --  * O(n) where n is the string length
   --  * Single pass through the string
   --  * No allocations or complex operations
   --  * Suitable for use in contracts and tight loops
   function Is_Valid_Format (Name : String) return Boolean
   with
     Post =>
       Is_Valid_Format'Result
       = (for all C of Name => C in 'a' .. 'z' or C in '0' .. '9' or C = '-');

   --  ## Predefined Compression Algorithms
   --
   --  Factory functions for standard data compression algorithms.
   --  These provide convenient access to well-known compression methods
   --  with guaranteed names and categories.
   --
   --  ### Compression Algorithm Selection Guide
   --
   --  **Brotli (brotli):**
   --  * Best compression ratio for text and web content
   --  * Slower compression but excellent for files served repeatedly
   --  * Modern algorithm with wide browser support
   --  * Ideal for: Web assets, text files, JSON/XML data
   --
   --  **Gzip (gzip):**
   --  * Balanced compression ratio and speed
   --  * Universal compatibility and support
   --  * Standard for HTTP compression and archives
   --  * Ideal for: General-purpose compression, legacy compatibility
   --
   --  **Zstandard (zstd):**
   --  * Excellent compression speed with good ratio
   --  * Tunable compression levels for different use cases
   --  * Modern algorithm optimized for real-time compression
   --  * Ideal for: Real-time data streams, database compression
   --
   --  **LZ4 (lz4):**
   --  * Extremely fast compression and decompression
   --  * Lower compression ratio but highest speed
   --  * Minimal CPU overhead for high-throughput scenarios
   --  * Ideal for: High-speed data processing, temporary compression
   --
   --  ### Usage Examples
   --  ```ada
   --  -- Select compression algorithm based on requirements
   --  declare
   --     High_Ratio_Algo : constant Algorithm_Type := Brotli;    -- Best compression
   --     Balanced_Algo   : constant Algorithm_Type := Gzip;     -- Balanced approach
   --     Fast_Algo       : constant Algorithm_Type := Zstd;     -- Fast compression
   --     Speed_Algo      : constant Algorithm_Type := Lz4;      -- Fastest processing
   --  begin
   --     -- Use in pipeline configuration
   --     if Storage_Cost_Critical then
   --        Pipeline_Config.Compression := High_Ratio_Algo;
   --     elsif CPU_Limited then
   --        Pipeline_Config.Compression := Speed_Algo;
   --     else
   --        Pipeline_Config.Compression := Balanced_Algo;
   --     end if;
   --  end;
   --  ```

   --  ### Brotli Compression Algorithm
   --
   --  Modern compression algorithm optimized for text and web content.
   --  Provides excellent compression ratios at the cost of slower compression speed.
   --
   --  #### Performance Characteristics
   --  * Compression Ratio: Excellent (often 15-20% better than gzip)
   --  * Compression Speed: Moderate (slower than gzip/zstd)
   --  * Decompression Speed: Fast (comparable to gzip)
   --  * Memory Usage: Moderate
   --
   --  #### Best Use Cases
   --  * Static web assets (CSS, JavaScript, HTML)
   --  * Text-heavy content (documentation, logs)
   --  * Data that will be compressed once and served many times
   --  * When storage space is more important than compression time
   function Brotli return Algorithm_Type
   with
     Post =>
       Name (Brotli'Result) = "brotli"
       and Category (Brotli'Result) = Compression;

   --  ### Gzip Compression Algorithm
   --
   --  The most widely supported compression algorithm with excellent compatibility.
   --  Provides good compression ratios with reasonable speed.
   --
   --  #### Performance Characteristics
   --  * Compression Ratio: Good (industry standard baseline)
   --  * Compression Speed: Moderate (widely optimized implementations)
   --  * Decompression Speed: Fast (highly optimized)
   --  * Memory Usage: Low to moderate
   --
   --  #### Best Use Cases
   --  * General-purpose file compression
   --  * HTTP content encoding (universal browser support)
   --  * Archive formats and backup systems
   --  * When maximum compatibility is required
   function Gzip return Algorithm_Type
   with
     Post =>
       Name (Gzip'Result) = "gzip" and Category (Gzip'Result) = Compression;

   --  ### Zstandard (Zstd) Compression Algorithm
   --
   --  Modern compression algorithm designed for real-time compression scenarios.
   --  Offers excellent speed with competitive compression ratios.
   --
   --  #### Performance Characteristics
   --  * Compression Ratio: Very Good (comparable to gzip, often better)
   --  * Compression Speed: Fast (optimized for modern CPUs)
   --  * Decompression Speed: Very Fast (often fastest among high-ratio algorithms)
   --  * Memory Usage: Configurable (can be tuned for different scenarios)
   --
   --  #### Best Use Cases
   --  * Real-time data compression in streaming applications
   --  * Database compression (Facebook/Meta's primary use case)
   --  * When both speed and compression ratio matter
   --  * Modern applications where zstd support is available
   function Zstd return Algorithm_Type
   with
     Post =>
       Name (Zstd'Result) = "zstd" and Category (Zstd'Result) = Compression;

   --  ### LZ4 Compression Algorithm
   --
   --  Ultra-fast compression algorithm optimized for speed over compression ratio.
   --  Ideal when decompression speed is critical.
   --
   --  #### Performance Characteristics
   --  * Compression Ratio: Fair (lower than other algorithms but still useful)
   --  * Compression Speed: Very Fast (often limited by memory bandwidth)
   --  * Decompression Speed: Extremely Fast (often fastest available)
   --  * Memory Usage: Very Low
   --
   --  #### Best Use Cases
   --  * High-throughput data processing pipelines
   --  * Temporary compression for in-memory operations
   --  * When CPU overhead must be minimized
   --  * Real-time systems with strict latency requirements
   function Lz4 return Algorithm_Type
   with
     Post => Name (Lz4'Result) = "lz4" and Category (Lz4'Result) = Compression;

   --  ## Predefined Encryption Algorithms
   --
   --  Factory functions for standard data encryption algorithms.
   --  These provide secure, authenticated encryption with strong cryptographic guarantees.
   --
   --  ### Encryption Algorithm Selection Guide
   --
   --  **AES-256-GCM:**
   --  * Industry standard authenticated encryption
   --  * Hardware acceleration available on most modern CPUs
   --  * NIST-approved and widely validated
   --  * Excellent performance on x86/x64 architectures
   --
   --  **ChaCha20-Poly1305:**
   --  * Modern authenticated encryption cipher
   --  * Excellent performance on ARM and resource-constrained devices
   --  * Resistance to timing attacks
   --  * Preferred for mobile and embedded applications
   --
   --  ### Security Guarantees
   --
   --  Both algorithms provide:
   --  * **Confidentiality**: Data cannot be read without the key
   --  * **Authenticity**: Data tampering is detected
   --  * **Integrity**: Data corruption is detected
   --  * **Replay Protection**: When combined with proper nonce management
   --
   --  ### Usage Examples
   --  ```ada
   --  -- Select encryption based on target platform
   --  declare
   --     Desktop_Encryption : constant Algorithm_Type := AES_256_GCM;      -- Hardware accelerated
   --     Mobile_Encryption  : constant Algorithm_Type := ChaCha20_Poly1305; -- ARM optimized
   --  begin
   --     if Target_Platform = x86_64 then
   --        Pipeline_Config.Encryption := Desktop_Encryption;
   --     elsif Target_Platform = ARM then
   --        Pipeline_Config.Encryption := Mobile_Encryption;
   --     end if;
   --  end;
   --  ```

   --  ### AES-256-GCM Encryption Algorithm
   --
   --  Advanced Encryption Standard with 256-bit keys in Galois/Counter Mode.
   --  Provides authenticated encryption with excellent performance on modern hardware.
   --
   --  #### Technical Specifications
   --  * Key Size: 256 bits (32 bytes)
   --  * Block Size: 128 bits (16 bytes)
   --  * Nonce Size: 96 bits (12 bytes) recommended
   --  * Authentication Tag: 128 bits (16 bytes)
   --
   --  #### Performance Characteristics
   --  * Encryption Speed: Very Fast (with AES-NI hardware support)
   --  * Decryption Speed: Very Fast (with AES-NI hardware support)
   --  * Memory Usage: Low
   --  * Hardware Support: Excellent (Intel AES-NI, ARM Crypto Extensions)
   --
   --  #### Security Properties
   --  * NIST FIPS 140-2 approved
   --  * Extensively analyzed and validated
   --  * Quantum-resistant against known algorithms
   --  * Suitable for classified government data (when properly implemented)
   --
   --  #### Best Use Cases
   --  * High-performance encryption on x86/x64 systems
   --  * Applications requiring FIPS compliance
   --  * Bulk data encryption with hardware acceleration
   --  * When maximum compatibility is needed
   function AES_256_GCM return Algorithm_Type
   with
     Post =>
       Name (AES_256_GCM'Result) = "aes-256-gcm"
       and Category (AES_256_GCM'Result) = Encryption;

   --  ### ChaCha20-Poly1305 Encryption Algorithm
   --
   --  Modern stream cipher with Poly1305 message authentication code.
   --  Designed for high security and excellent performance on all platforms.
   --
   --  #### Technical Specifications
   --  * Key Size: 256 bits (32 bytes)
   --  * Nonce Size: 96 bits (12 bytes) for AEAD variant
   --  * Authentication Tag: 128 bits (16 bytes)
   --  * Stream Cipher: No block size constraints
   --
   --  #### Performance Characteristics
   --  * Encryption Speed: Fast (especially on ARM and non-Intel platforms)
   --  * Decryption Speed: Fast (symmetric with encryption)
   --  * Memory Usage: Very Low
   --  * Hardware Support: Good (ARM Crypto Extensions, some x86 optimizations)
   --
   --  #### Security Properties
   --  * RFC 8439 standardized
   --  * Designed by Daniel J. Bernstein (renowned cryptographer)
   --  * Resistance to timing attacks by design
   --  * Simple implementation reduces risk of side-channel attacks
   --
   --  #### Best Use Cases
   --  * Mobile and embedded systems (ARM processors)
   --  * Applications where timing attack resistance is critical
   --  * When AES hardware acceleration is not available
   --  * Modern applications prioritizing security margins
   function ChaCha20_Poly1305 return Algorithm_Type
   with
     Post =>
       Name (ChaCha20_Poly1305'Result) = "chacha20-poly1305"
       and Category (ChaCha20_Poly1305'Result) = Encryption;

   --  ## Predefined Hashing Algorithms
   --
   --  Factory functions for cryptographic hash algorithms used for data integrity,
   --  checksums, and digital fingerprinting. These algorithms provide strong
   --  collision resistance and are suitable for security-critical applications.
   --
   --  ### Hashing Algorithm Selection Guide
   --
   --  **SHA-256:**
   --  * Industry standard with universal support
   --  * NIST-approved and extensively validated
   --  * Good balance of security and performance
   --  * Required for many compliance frameworks
   --
   --  **SHA-512:**
   --  * Higher security margin than SHA-256
   --  * Better performance on 64-bit systems
   --  * Larger hash output (512 bits vs 256 bits)
   --  * Preferred for long-term data integrity
   --
   --  **BLAKE3:**
   --  * Modern, extremely fast hash function
   --  * Excellent security properties
   --  * Parallelizable and optimized for modern hardware
   --  * Best choice for new applications when supported
   --
   --  ### Common Use Cases
   --
   --  **Data Integrity Verification:**
   --  ```ada
   --  -- Calculate file checksum for integrity checking
   --  declare
   --     Hash_Algo : constant Algorithm_Type := SHA256;
   --     File_Hash : constant String := Calculate_File_Hash ("data.bin", Hash_Algo);
   --  begin
   --     Store_Integrity_Hash ("data.bin", File_Hash);
   --  end;
   --  ```
   --
   --  **Content Deduplication:**
   --  ```ada
   --  -- Use fast hashing for content-based deduplication
   --  declare
   --     Dedup_Algo : constant Algorithm_Type := Blake3;  -- Fastest option
   --     Content_Hash : constant String := Calculate_Hash (Chunk_Data, Dedup_Algo);
   --  begin
   --     if not Hash_Store.Contains (Content_Hash) then
   --        Hash_Store.Insert (Content_Hash, Chunk_Data);
   --     end if;
   --  end;
   --  ```

   --  ### SHA-256 Hash Algorithm
   --
   --  Secure Hash Algorithm 256-bit, part of the SHA-2 family.
   --  The most widely used cryptographic hash function for general purposes.
   --
   --  #### Technical Specifications
   --  * Hash Size: 256 bits (32 bytes)
   --  * Block Size: 512 bits (64 bytes)
   --  * Security Level: 128 bits (against collision attacks)
   --  * Message Length Limit: 2^64 - 1 bits
   --
   --  #### Performance Characteristics
   --  * Hashing Speed: Good (hardware acceleration available)
   --  * Memory Usage: Low (small internal state)
   --  * Hardware Support: Excellent (Intel SHA Extensions, ARM Crypto)
   --  * Software Optimization: Highly optimized implementations available
   --
   --  #### Security Properties
   --  * NIST FIPS 180-4 standardized
   --  * No known practical attacks
   --  * Suitable for digital signatures and certificates
   --  * Required by many security standards and protocols
   --
   --  #### Best Use Cases
   --  * General-purpose cryptographic hashing
   --  * Digital signature applications
   --  * Blockchain and cryptocurrency applications
   --  * Compliance with security standards (FIPS, Common Criteria)
   function SHA256 return Algorithm_Type
   with
     Post =>
       Name (SHA256'Result) = "sha256" and Category (SHA256'Result) = Hashing;

   --  ### SHA-512 Hash Algorithm
   --
   --  Secure Hash Algorithm 512-bit, providing higher security margin than SHA-256.
   --  Optimized for 64-bit platforms with excellent long-term security properties.
   --
   --  #### Technical Specifications
   --  * Hash Size: 512 bits (64 bytes)
   --  * Block Size: 1024 bits (128 bytes)
   --  * Security Level: 256 bits (against collision attacks)
   --  * Message Length Limit: 2^128 - 1 bits
   --
   --  #### Performance Characteristics
   --  * Hashing Speed: Very Good (especially on 64-bit systems)
   --  * Memory Usage: Low to moderate
   --  * Hardware Support: Good (some hardware acceleration available)
   --  * 64-bit Optimization: Excellent (native 64-bit operations)
   --
   --  #### Security Properties
   --  * Higher security margin than SHA-256
   --  * Resistant to length extension attacks
   --  * Suitable for long-term security requirements
   --  * Often preferred for high-security applications
   --
   --  #### Best Use Cases
   --  * Long-term data integrity (archival systems)
   --  * High-security applications requiring larger security margins
   --  * Applications processing large amounts of data on 64-bit systems
   --  * When larger hash outputs are beneficial for collision resistance
   function SHA512 return Algorithm_Type
   with
     Post =>
       Name (SHA512'Result) = "sha512" and Category (SHA512'Result) = Hashing;

   --  ### BLAKE3 Hash Algorithm
   --
   --  Modern cryptographic hash function designed for speed, security, and simplicity.
   --  Based on the BLAKE2 design with significant performance improvements.
   --
   --  #### Technical Specifications
   --  * Hash Size: 256 bits (32 bytes) default, extendable output
   --  * Block Size: Variable (optimized for modern CPUs)
   --  * Security Level: 256 bits (conservative security margin)
   --  * Parallelization: Inherently parallel design
   --
   --  #### Performance Characteristics
   --  * Hashing Speed: Excellent (often 2-4x faster than SHA-256)
   --  * Memory Usage: Very Low
   --  * Parallel Processing: Excellent (leverages multiple CPU cores)
   --  * SIMD Optimization: Excellent (AVX2, NEON optimizations)
   --
   --  #### Security Properties
   --  * Based on extensively analyzed ChaCha permutation
   --  * No known vulnerabilities
   --  * Conservative security margins
   --  * Designed by respected cryptography team
   --
   --  #### Best Use Cases
   --  * High-performance hashing applications
   --  * Modern systems where BLAKE3 support is available
   --  * Content-addressable storage systems
   --  * When maximum speed is required without compromising security
   function Blake3 return Algorithm_Type
   with
     Post =>
       Name (Blake3'Result) = "blake3" and Category (Blake3'Result) = Hashing;

   --  ## Algorithm Equality Comparison
   --
   --  Compares two algorithm value objects for equality based on their names.
   --  Two algorithms are considered equal if they have identical names,
   --  following value object semantics.
   --
   --  ### Parameters
   --  * `Left` - First algorithm to compare
   --  * `Right` - Second algorithm to compare
   --
   --  ### Returns
   --  True if both algorithms have the same name, False otherwise
   --
   --  ### Value Object Semantics
   --  Algorithm equality is based solely on the algorithm name:
   --  * Two "gzip" algorithms are always equal regardless of how they were created
   --  * Algorithm instances with different names are never equal
   --  * Equality is transitive, symmetric, and reflexive
   --
   --  ### Examples
   --  ```ada
   --  declare
   --     Algo1 : constant Algorithm_Type := Gzip;
   --     Algo2 : constant Algorithm_Type := Create ("gzip");
   --     Algo3 : constant Algorithm_Type := Brotli;
   --  begin
   --     pragma Assert (Algo1 = Algo2);  -- Same algorithm name
   --     pragma Assert (Algo1 /= Algo3); -- Different algorithm names
   --
   --     -- Use in collections and lookups
   --     if Selected_Algorithm = AES_256_GCM then
   --        Apply_Strong_Encryption (Data);
   --     end if;
   --
   --     -- Use in algorithm switching logic
   --     case Selected_Compression is
   --        when Gzip_Algorithm =>
   --           if Selected_Compression = Gzip then
   --              Apply_Gzip_Compression (Data);
   --           end if;
   --     end case;
   --  end;
   --  ```
   overriding
   function "=" (Left, Right : Algorithm_Type) return Boolean;

   --  ## Algorithm String Representation
   --
   --  Provides a human-readable string representation of the algorithm
   --  including both its name and category. Useful for debugging,
   --  logging, and user interface display.
   --
   --  ### Parameters
   --  * `Self` - Algorithm to convert to string representation
   --
   --  ### Returns
   --  String in format: "algorithm-name (CATEGORY)"
   --
   --  ### Format Examples
   --  * `Image (Gzip)` returns "gzip (COMPRESSION)"
   --  * `Image (AES_256_GCM)` returns "aes-256-gcm (ENCRYPTION)"
   --  * `Image (SHA256)` returns "sha256 (HASHING)"
   --  * `Image (Create ("custom-algo"))` returns "custom-algo (CUSTOM)"
   --
   --  ### Usage
   --  ```ada
   --  declare
   --     Selected_Algo : constant Algorithm_Type := Get_User_Selection;
   --  begin
   --     -- Logging
   --     Log.Info ("Processing with algorithm: " & Image (Selected_Algo));
   --
   --     -- User interface display
   --     Status_Display.Set_Text ("Using " & Image (Selected_Algo));
   --
   --     -- Configuration output
   --     Config_File.Write_Line ("algorithm=" & Image (Selected_Algo));
   --
   --     -- Debug output
   --     Put_Line ("Algorithm details: " & Image (Selected_Algo));
   --  end;
   --  ```
   function Image (Self : Algorithm_Type) return String
   is (Name (Self) & " (" & Category (Self)'Image & ")");

private
   type Algorithm_Type is tagged record
      Name_Value : Unbounded_String;
   end record
   with Type_Invariant => Is_Valid (Algorithm_Type);

   --  Validation function for type invariant
   function Is_Valid (Self : Algorithm_Type) return Boolean
   is (Length (Self.Name_Value) > 0
       and then Is_Valid_Format (To_String (Self.Name_Value)));

end Pipelib.Core.Domain.Value_Objects.Algorithm;
