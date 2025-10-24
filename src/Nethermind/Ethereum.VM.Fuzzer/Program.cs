using System;

using Nethermind.Core;
using Nethermind.Int256;

namespace Ethereum.VM.Fuzzer;

public class Program
{
    public static void Main(string[] args) =>
        SharpFuzz.Fuzzer.LibFuzzer.Run(Harness);

    public static void Harness(ReadOnlySpan<byte> data)
    {
        try
        {
            // Create a simple transaction with the fuzzed bytecode.
            var transaction = new Transaction
            {
                Data = data.ToArray(),
                Value = UInt256.Zero,
                GasLimit = 1000000, // Large gas limit for fuzzing.
                GasPrice = UInt256.One,
                To = null, // Contract creation.
                Nonce = 0
            };

            // Simple validation - just check if transaction is valid.
            if (transaction.Data.Length > 0)
            {
                byte[] bytecode = transaction.Data.ToArray();

                // Check for basic EVM instruction patterns.
                for (int i = 0; i < bytecode.Length; i++)
                {
                    byte opcode = bytecode[i];

                    // Check if it's a valid EVM opcode (0x00-0xFF).
                    if (opcode > 0xFF)
                    {
                        throw new InvalidOperationException(
                            $"Invalid opcode: {opcode:X2}");
                    }

                    // Check for PUSH instructions and validate their data.
                    if (opcode >= 0x60 && opcode <= 0x7F) // PUSH1 to PUSH32.
                    {
                        int pushSize = opcode - 0x60 + 1;

                        if (i + pushSize >= bytecode.Length)
                        {
                            throw new InvalidOperationException(
                                $"PUSH instruction extends beyond bytecode");
                        }

                        // Skip the pushed data.
                        i += pushSize;
                    }
                }
            }
        }
        catch (Exception)
        {
            // Unexpected exception - potential bug.
            throw;
        }
    }
}
