# 🍽️ Dynamic Menu Pricing

A smart contract that implements dynamic pricing for restaurant menus with surge pricing during peak hours and customer voting mechanisms.

## 🚀 Features

- **📈 Dynamic Pricing**: Automatically adjusts prices based on demand and time of day
- **⏰ Surge Pricing**: Higher prices during peak hours (11AM-2PM, 6PM-9PM)
- **🗳️ Customer Voting**: Customers can rate menu items (1-5 stars) affecting pricing
- **📊 Demand Analytics**: Track daily orders and revenue per item
- **👨‍💼 Admin Controls**: Restaurant owners can manage menu items and pricing settings
- **🔧 Customizable**: Set custom hourly multipliers and surge pricing settings

## 🏗️ Contract Architecture

### Data Structures
- **Menu Items**: Store item details, base prices, categories, and voting data
- **Customer Votes**: Track customer ratings for menu items
- **Daily Demand**: Monitor orders and revenue per item per day
- **Hourly Multipliers**: Custom pricing multipliers for specific hours

### Price Calculation
Final price = Base Price × Surge Multiplier × Demand Multiplier × Vote Multiplier

## 📋 Usage Instructions

### Owner Functions

#### Add Menu Item
```clarity
(contract-call? .dynamic-menu-pricing add-menu-item "Burger" u1000 "Main")
```

#### Update Menu Item
```clarity
(contract-call? .dynamic-menu-pricing update-menu-item u1 "Deluxe Burger" u1200 "Main")
```

#### Toggle Item Active Status
```clarity
(contract-call? .dynamic-menu-pricing toggle-menu-item u1)
```

#### Set Surge Multiplier (100-300%)
```clarity
(contract-call? .dynamic-menu-pricing set-surge-multiplier u150)
```

#### Set Hourly Multiplier
```clarity
(contract-call? .dynamic-menu-pricing set-hourly-multiplier u12 u130)
```

### Customer Functions

#### Vote for Item (1-5 stars)
```clarity
(contract-call? .dynamic-menu-pricing vote-for-item u1 u5)
```

#### Record Order
```clarity
(contract-call? .dynamic-menu-pricing record-order u1 u2)
```

### Read-Only Functions

#### Get Menu Item Details
```clarity
(contract-call? .dynamic-menu-pricing get-menu-item u1)
```

#### Get Dynamic Price
```clarity
(contract-call? .dynamic-menu-pricing get-dynamic-price u1)
```

#### Get Item Analytics
```clarity
(contract-call? .dynamic-menu-pricing get-item-analytics u1)
```

#### Get Current Surge Multiplier
```clarity
(contract-call? .dynamic-menu-pricing get-surge-multiplier)
```

## ⚙️ Configuration

### Peak Hours
- **Lunch**: 11AM - 2PM (blocks 11-14)
- **Dinner**: 6PM - 9PM (blocks 18-21)

### Pricing Multipliers
- **Base Surge**: 150% (configurable)
- **High Demand**: 120% (>10 orders/day)
- **Medium Demand**: 110% (5-10 orders/day)
- **Low Demand**: 100% (<5 orders/day)

### Vote Impact
- **Excellent** (≥4 stars): 110% multiplier
- **Good** (≥3 stars): 100% multiplier
- **Poor** (<3 stars): 95% multiplier

## 🛠️ Development

### Prerequisites
- Clarinet CLI
- Node.js (for testing)

### Setup
```bash
clarinet check
clarinet test
```

### Testing
```bash
clarinet test tests/dynamic-menu-pricing_test.ts
```

## 📊 Example Scenarios

### 🌅 Morning (Low Demand)
- Base price: $10.00
- No surge: 100%
- Low demand: 100%
- Average rating: 100%
- **Final price: $10.00**

### 🍽️ Lunch Rush (High Demand)
- Base price: $10.00
- Surge pricing: 150%
- High demand: 120%
- Excellent rating: 110%
- **Final price: $19.80**

### 🌙 Evening (Medium Demand)
- Base price: $10.00
- Surge pricing: 150%
- Medium demand: 110%
- Good rating: 100%
- **Final price: $16.50**

## 🔒 Security Features

- Owner-only administrative functions
- Input validation for all parameters
- Prevents duplicate voting
- Price bounds checking
- Active item validation

## 📈 Analytics Dashboard

The contract provides comprehensive analytics:
- Real-time pricing calculations
- Daily demand tracking
- Customer voting trends
- Revenue monitoring
- Peak hour performance

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests with `clarinet test`
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License.

---

*Built with ❤️ using Clarity and Stacks blockchain*
