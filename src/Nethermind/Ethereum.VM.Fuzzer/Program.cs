using System;

using Ethereum.Test.Base;

using Nethermind.Blockchain;
using Nethermind.Core;
using Nethermind.Core.Crypto;
using Nethermind.Core.Test.Db;
using Nethermind.Db;
using Nethermind.Evm;
using Nethermind.Evm.Tracing;
using Nethermind.Evm.TransactionProcessing;
using Nethermind.Int256;
using Nethermind.Logging;
using Nethermind.Specs;
using Nethermind.State;
using Nethermind.Trie;
using Nethermind.Trie.Pruning;

namespace Ethereum.VM.Fuzzer;

public class Program
{
    public static void Main(string[] args) =>
        SharpFuzz.Fuzzer.LibFuzzer.Run(Harness);

    public static void Harness(ReadOnlySpan<byte> data)
    {
        try
        {
            // Create a transaction with the fuzzed bytecode for contract creation.
            var transaction = new Transaction
            {
                Data          = data.ToArray(),
                Value         = UInt256.Zero,
                GasLimit      = 1000000,
                GasPrice      = UInt256.One,
                To            = null,
                Nonce         = 0,
                SenderAddress = Address.Zero
            };

            // Setup EVM execution environment.
            MainnetSpecProvider specProvider = MainnetSpecProvider.Instance;
            LimboLogs logManager = LimboLogs.Instance;

            // Create simple in-memory world state.
            IDbProvider memDbProvider = TestMemDbProvider.Init();
            var trieStore = new TrieStore(
                new NodeStorage(memDbProvider.StateDb),
                No.Pruning,
                Persist.EveryBlock,
                new PruningConfig(),
                logManager);
            var worldState = new WorldState(
                trieStore,
                memDbProvider.CodeDb,
                logManager);

            // Create virtual machine.
            var virtualMachine = new VirtualMachine(
                new TestBlockhashProvider(),
                specProvider,
                logManager);

            // Create code info repository.
            var codeInfoRepository = new EthereumCodeInfoRepository(
                worldState);

            // Create transaction processor.
            var transactionProcessor = new TransactionProcessor(
                BlobBaseFeeCalculator.Instance,
                specProvider,
                worldState,
                virtualMachine,
                codeInfoRepository,
                logManager);

            // Create a simple block header
            var blockHeader = new BlockHeader(
                Keccak.Zero,
                Keccak.Zero,
                Address.Zero,
                UInt256.Zero,
                0,
                1000000,
                0,
                []);

            // Execute the transaction using NullTxTracer (simplest option).
            TransactionResult result = transactionProcessor.Execute(
                transaction,
                blockHeader,
                NullTxTracer.Instance);

            // Check if execution was successful or failed gracefully.
            // We expect some executions to fail - that's normal for random bytecode.
            if (!result.TransactionExecuted)
            {
                // This is expected for invalid bytecode - don't crash.
            }
        }
        catch (EvmException)
        {
            // Expected for invalid bytecode - don't crash.
        }
        catch (Exception ex) when (ex is not EvmException)
        {
            // Unexpected exception - potential bug.
            throw;
        }
    }
}
