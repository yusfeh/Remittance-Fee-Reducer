# 💸 Remittance Fee Reducer

> P2P remittance system built on Bitcoin via Clarity, eliminating traditional banking fees

## 🌟 Overview

The Remittance Fee Reducer enables direct peer-to-peer money transfers with minimal fees, bypassing traditional banking intermediaries. Built on the Stacks blockchain using Clarity smart contracts, it provides secure escrow functionality and optional agent facilitation for international transfers.

## ✨ Key Features

- 🔒 **Secure Escrow**: Automated escrow system protects both sender and recipient
- 💰 **Low Fees**: Base fee of 2.5% with optional agent commission
- 🌐 **Agent Network**: Verified agents assist with currency conversion and local payout
- ⚡ **Fast Transfers**: Direct P2P transfers without banking delays
- 🛡️ **Dispute Resolution**: Built-in dispute mechanism with contract owner arbitration
- 📊 **Reputation System**: User and agent reputation tracking for trust building
- 🔄 **Batch Transfers**: Send to multiple recipients in a single transaction

## 🚀 Quick Start

### Prerequisites
- Clarinet CLI installed
- Stacks wallet with STX tokens

### Setup
```bash
git clone <repository-url>
cd Remittance-Fee-Reducer
clarinet check
clarinet test
```

## 📋 Contract Functions

### 👤 User Registration
```clarity
(contract-call? .Remittance-Fee-Reducer register-user false)
```

### 🏢 Agent Registration
```clarity
(contract-call? .Remittance-Fee-Reducer register-agent u200 (list "USD" "EUR" "GBP"))
```

### 💸 Initiate Transfer
```clarity
(contract-call? .Remittance-Fee-Reducer initiate-transfer 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 u1000000 none)
```

### ✅ Complete Transfer
```clarity
(contract-call? .Remittance-Fee-Reducer complete-transfer u1 "CONF123456")
```

### ❌ Cancel Transfer
```clarity
(contract-call? .Remittance-Fee-Reducer cancel-transfer u1)
```

## 🔍 Read-Only Functions

### Get Transfer Details
```clarity
(contract-call? .Remittance-Fee-Reducer get-transfer u1)
```

### Check User Profile
```clarity
(contract-call? .Remittance-Fee-Reducer get-user-profile 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

### Calculate Transfer Cost
```clarity
(contract-call? .Remittance-Fee-Reducer get-transfer-cost u1000000)
```

### Get Contract Statistics
```clarity
(contract-call? .Remittance-Fee-Reducer get-contract-stats)
```

## 💼 Agent Features

Agents are verified intermediaries who help facilitate transfers, especially for currency conversion and local payouts.

### Agent Benefits
- Earn commission on facilitated transfers
- Build reputation through successful transactions
- Support multiple currencies
- Access to priority transfer matching

### Agent Requirements
- Commission rate ≤ 5%
- Maintain active status
- Provide reliable service for reputation building

## 🔄 Transfer Lifecycle

1. **Initiation**: Sender initiates transfer with amount and recipient
2. **Escrow**: Funds locked in smart contract escrow
3. **Agent Matching** (optional): Agent facilitates currency conversion
4. **Completion**: Recipient confirms receipt with completion code
5. **Payout**: Funds released to recipient, fees distributed

## 🛡️ Security Features

- **Timeout Protection**: Transfers auto-expire after 1008 blocks (~7 days)
- **Dispute Resolution**: Built-in arbitration system
- **Reputation Tracking**: User and agent reputation scores
- **KYC Integration**: Optional KYC verification support
- **Emergency Controls**: Contract pause functionality

## 💱 Fee Structure

- **Base Fee**: 2.5% of transfer amount (minimum 0.01 STX)
- **Agent Commission**: Up to 5% of base fee (paid to facilitating agent)
- **Total Maximum Fee**: ~2.625% of transfer amount

## 🔧 Configuration

### Exchange Rates (Owner Only)
```clarity
(contract-call? .Remittance-Fee-Reducer update-exchange-rate "USDSTX" u50000)
```

### Emergency Pause (Owner Only)
```clarity
(contract-call? .Remittance-Fee-Reducer pause-contract true)
```

## 📊 Analytics

Track your transfer history and contract performance:
- Total volume processed
- Fee collection metrics
- User reputation scores
- Agent performance statistics

## 🧪 Testing

Run the test suite:
```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Run tests with `clarinet test`
4. Submit a pull request

## 📄 License

MIT License - see LICENSE file for details

## 🆘 Support

For technical support or questions about the remittance system, please open an issue in the repository.

---

*Built with ❤️ on Stacks blockchain*
