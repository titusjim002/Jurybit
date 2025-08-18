# ⚖️ Jurybit - Decentralized Jury Pool Protocol

A smart contract protocol that randomly selects community members to arbitrate disputes on the Stacks blockchain.

## 🎯 Overview

Jurybit enables decentralized dispute resolution by creating a pool of registered jurors who can be randomly selected to vote on cases. Community members stake STX to become jurors and earn rewards for participating in case arbitration.

## ✨ Features

- 👥 **Juror Registration**: Community members can register as jurors by staking STX
- 🎲 **Random Selection**: Jurors are randomly selected for each case
- 🗳️ **Voting System**: Selected jurors vote on dispute outcomes
- 💰 **Reward Distribution**: Winners and participating jurors receive rewards
- 📊 **Reputation System**: Jurors build reputation through participation
- ⏰ **Time-bound Cases**: Cases have defined voting periods

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- STX tokens for staking and case fees

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Deploy the contract using Clarinet

```bash
clarinet deploy
```

## 📖 Usage

### Becoming a Juror

Register as a juror by staking at least 1 STX:

```clarity
(contract-call? .Jurybit register-as-juror u1000000)
```

### Creating a Case

Create a dispute case with a defendant and description:

```clarity
(contract-call? .Jurybit create-case 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 "Contract dispute over payment" u500000)
```

### Voting on Cases

If selected as a juror, vote on active cases:

```clarity
(contract-call? .Jurybit vote-on-case u1 "plaintiff")
```

### Finalizing Cases

After the voting period ends, anyone can finalize the case:

```clarity
(contract-call? .Jurybit finalize-case u1)
```

## 🔧 Contract Functions

### Public Functions

| Function | Description |
|----------|-------------|
| `register-as-juror` | Register as a juror with STX stake |
| `create-case` | Create a new dispute case |
| `vote-on-case` | Vote on an active case (jurors only) |
| `finalize-case` | Finalize case after voting period |
| `withdraw-stake` | Withdraw stake and leave jury pool |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-case` | Get case details by ID |
| `get-juror` | Get juror information |
| `get-case-jury` | Get jury members for a case |
| `get-jury-vote` | Get specific juror's vote |
| `get-total-jurors` | Get total number of registered jurors |
| `is-case-active` | Check if a case is still active |

## 💡 Key Parameters

- **Minimum Stake**: 1 STX (1,000,000 microSTX)
- **Jury Size**: 5 jurors per case
- **Voting Period**: 144 blocks (~24 hours)
- **Case Fee**: 0.5 STX minimum

## 🏆 Rewards System

- **Case Winners**: Receive 50% of the case stake
- **Jurors**: Share 50% of the case stake equally
- **Reputation**: Jurors gain reputation points for participation
