package org.quasigroup.ibclient

import org.quasigroup.ibclient.types.*

trait ContractSpec:

  val USStockWithPrimaryExch =
    Contract(symbol = "SPY", secType = SecType.STK, currency = "USD", exchange = "SMART", primaryExch = "ARCA")

  val BondWithCusip =
    Contract(symbol = "912828C57", secType = SecType.BOND, currency = "USD", exchange = "SMART")

  val Bond =
    Contract(conId = 456467716, exchange = "SMART")

  val MutualFund =
    Contract(symbol = "VINIX", secType = SecType.FUND, currency = "USD", exchange = "FUNDSERV")

  val Commodity =
    Contract(symbol = "XAUUSD", secType = SecType.CMDTY, currency = "USD", exchange = "SMART")

  val EurGbpFx =
    Contract(symbol = "EUR", secType = SecType.CASH, currency = "GBP", exchange = "IDEALPRO")

  val Index =
    Contract(symbol = "DAX", secType = SecType.IND, currency = "EUR", exchange = "EUREX")

  val CFD =
    Contract(symbol = "IBDE30", secType = SecType.CFD, currency = "EUR", exchange = "SMART")

  val USStockCFD =
    Contract(symbol = "IBM", secType = SecType.CFD, currency = "USD", exchange = "SMART")

  val EuropeanStockCFD =
    Contract(symbol = "BMW", secType = SecType.CFD, currency = "EUR", exchange = "SMART")

  val CashCFD =
    Contract(symbol = "EUR", secType = SecType.CFD, currency = "USD", exchange = "SMART")

  val EuropeanStock =
    Contract(symbol = "NOKIA", secType = SecType.STK, currency = "EUR", exchange = "SMART", primaryExch = "HEX")

  val OptionAtIse =
    Contract(
      symbol = "BPX",
      secType = SecType.OPT,
      currency = "USD",
      exchange = "SMART",
      lastTradeDateOrContractMonth = "20160916",
      right = ContractRight.Call,
      strike = 65,
      multiplier = "100"
    )

  val USStock =
    Contract(symbol = "SPY", secType = SecType.STK, currency = "USD", exchange = "ARCA")

  val USStockAtSmart =
    Contract(
      symbol = "IBM",
      secType = SecType.STK,
      currency = "USD",
      exchange = "SMART"
    )

  val etf =
    Contract(
      symbol = "QQQ",
      secType = SecType.STK,
      currency = "USD",
      exchange = "SMART"
    )

  val USOptionContract =
    Contract(
      symbol = "GOOG",
      secType = SecType.OPT,
      currency = "USD",
      exchange = "SMART",
      lastTradeDateOrContractMonth = "20170120",
      strike = 615,
      right = ContractRight.Call,
      multiplier = "100"
    )

  val OptionAtBOX =
    Contract(
      symbol = "GOOG",
      secType = SecType.OPT,
      currency = "USD",
      exchange = "BOX",
      lastTradeDateOrContractMonth = "20170120",
      right = ContractRight.Call,
      strike = 615,
      multiplier = "100"
    )

  val OptionWithTradingClass =
    Contract(
      symbol = "SANT",
      secType = SecType.OPT,
      currency = "EUR",
      exchange = "MEFFRV",
      lastTradeDateOrContractMonth = "20190621",
      right = ContractRight.Call,
      strike = 7.5,
      multiplier = "100",
      tradingClass = "SANEU"
    )

  val OptionWithLocalSymbol =
    Contract(
      // Watch out for the spaces within the local symbol!
      localSymbol = "P BMW  20221216 72 M",
      secType = SecType.OPT,
      exchange = "EUREX",
      currency = "EUR"
    )

  val DutchWarrant =
    Contract(
      localSymbol = "B881G",
      secType = SecType.IOPT,
      exchange = "SBF",
      currency = "EUR"
    )

  val SimpleFuture =
    Contract(
      symbol = "GBL",
      secType = SecType.FUT,
      currency = "EUR",
      exchange = "EUREX",
      lastTradeDateOrContractMonth = "202303"
    )

  val FutureWithLocalSymbol =
    Contract(
      localSymbol = "FGBL MAR 23",
      secType = SecType.FUT,
      currency = "EUR",
      exchange = "EUREX"
    )

  val FutureWithMultiplier =
    Contract(
      symbol = "DAX",
      secType = SecType.FUT,
      currency = "EUR",
      exchange = "EUREX",
      lastTradeDateOrContractMonth = "202303",
      multiplier = "1"
    )

  val WrongContract =
    Contract(
      localSymbol = " IJR ",
      conId = 9579976,
      secType = SecType.STK,
      currency = "USD",
      exchange = "SMART"
    )

  val FuturesOnOptions =
    Contract(
      symbol = "GBL",
      secType = SecType.FOP,
      currency = "EUR",
      exchange = "EUREX",
      lastTradeDateOrContractMonth = "20230224",
      right = ContractRight.Call,
      strike = 138,
      multiplier = "1000"
    )

  val Warrants =
    Contract(
      symbol = "GOOG",
      secType = SecType.WAR,
      currency = "EUR",
      exchange = "FWB",
      lastTradeDateOrContractMonth = "20201117",
      right = ContractRight.Call,
      strike = 1500.0,
      multiplier = "0.01"
    )

  val ByISIN =
    Contract(
      secIdType = SecIdType.ISIN,
      secId = "US45841N1072",
      secType = SecType.STK,
      currency = "USD",
      exchange = "SMART"
    )

  val ByconId =
    Contract(
      conId = 12087792,
      secType = SecType.CASH,
      exchange = "IDEALPRO"
    )

  val OptionForQuery =
    Contract(
      symbol = "FISV",
      secType = SecType.OPT,
      currency = "USD",
      exchange = "SMART"
    )

  val OptionComboContract =
    Contract(
      symbol = "DBK",
      secType = SecType.BAG,
      currency = "EUR",
      exchange = "EUREX",
      comboLegs = List(
        ComboLeg(
          conId = 577164786, // DBK Jun21'24 2 CALL @EUREX
          ratio = 1,
          action = Action.BUY,
          exchange = "EUREX"
        ),
        ComboLeg(
          conId = 577164767, // DBK Dec15'23 2 CALL @EUREX
          ratio = 1,
          action = Action.SELL,
          exchange = "EUREX"
        )
      )
    )

  val StockComboContract =
    Contract(
      symbol = "MCD",
      secType = SecType.BAG,
      currency = "USD",
      exchange = "SMART",
      comboLegs = List(
        ComboLeg(
          conId = 43645865, // IBKR STK
          ratio = 1,
          action = Action.BUY,
          exchange = "SMART"
        ),
        ComboLeg(
          conId = 9408, // MCD STK
          ratio = 1,
          action = Action.SELL,
          exchange = "SMART"
        )
      )
    )

  val FutureComboContract =

    Contract(
      symbol = "VIX",
      secType = SecType.BAG,
      currency = "USD",
      exchange = "CFE",
      comboLegs = List(
        ComboLeg(
          conId = 195538625, // VIX FUT 20160217
          ratio = 1,
          action = Action.BUY,
          exchange = "CFE"
        ),
        ComboLeg(
          conId = 197436571, // VIX FUT 20160316
          ratio = 1,
          action = Action.SELL,
          exchange = "CFE"
        )
      )
    )

    val SmartFutureComboContract =
      Contract(
        symbol = "WTI", // WTI,COIL spread. Symbol can be defined as first leg symbol ("WTI") or currency ("USD").
        secType = SecType.BAG,
        currency = "USD",
        exchange = "SMART", // smart-routed rather than direct routed
        comboLegs = List(
          ComboLeg(
            conId = 55928698, // WTI future June 2017
            ratio = 1,
            action = Action.BUY,
            exchange = "IPE"
          ),
          ComboLeg(
            conId = 55850663, // COIL future June 2017
            ratio = 1,
            action = Action.SELL,
            exchange = "IPE"
          )
        )
      )

  val InterCmdtyFuturesContract =
    Contract(
      symbol = "COIL.WTI",
      secType = SecType.BAG,
      currency = "USD",
      exchange = "IPE",
      comboLegs = List(
        ComboLeg(
          conId = 183405603,
          ratio = 1,
          action = Action.BUY,
          exchange = "IPE"
        ),
        ComboLeg(
          conId = 254011009,
          ratio = 1,
          action = Action.SELL,
          exchange = "IPE"
        )
      )
    )

  val NewsFeedForQuery =
    Contract(
      secType = SecType.NEWS,
      exchange = "BRF" // Briefing Trader

    )

  val BTbroadtapeNewsFeed =

    Contract(
      symbol = "BRF:BRF_ALL", // BroadTape All News
      secType = SecType.NEWS,
      exchange = "BRF" // Briefing Trader

    )

    val BZbroadtapeNewsFeed =
      Contract(
        symbol = "BZ:BZ_ALL", // BroadTape All News
        secType = SecType.NEWS,
        exchange = "BZ" // Benzinga Pro

      )

    val FLYbroadtapeNewsFeed =
      Contract(
        symbol = "FLY:FLY_ALL", // BroadTape All News
        secType = SecType.NEWS,
        exchange = "FLY" // Fly on the Wall

      )

  val ContFut =
    Contract(
      symbol = "GBL",
      secType = SecType.CONTFUT,
      exchange = "EUREX"
    )

  val ContAndExpiringFut =
    Contract(
      symbol = "GBL",
      secType = SecType.FUT_CONTFUT,
      exchange = "EUREX"
    )

  val JefferiesContract =
    Contract(
      symbol = "AAPL",
      secType = SecType.STK,
      exchange = "JEFFALGO", // must be direct-routed to JEFFALGO
      currency = "USD" // only available for US stocks

    )

  val CSFBContract =
    Contract(
      symbol = "IBKR",
      secType = SecType.STK,
      exchange = "CSFBALGO", // must be direct-routed to CSFBALGO
      currency = "USD" // only available for US stocks

    )

  val QBAlgoContract =
    Contract(
      symbol = "ES",
      secType = SecType.FUT,
      exchange = "QBALGO",
      currency = "USD",
      lastTradeDateOrContractMonth = "202003"
    )

  val IBKRATSContract =
    Contract(
      symbol = "SPY",
      secType = SecType.STK,
      exchange = "IBKRATS",
      currency = "USD"
    )

  val CryptoContract =
    Contract(
      symbol = "ETH",
      secType = SecType.CRYPTO,
      exchange = "PAXOS",
      currency = "USD"
    )

  val StockWithIPOPrice =
    Contract(
      symbol = "EMCGU",
      secType = SecType.STK,
      currency = "USD",
      exchange = "SMART"
    )

  val ByFIGI =
    Contract(
      secIdType = SecIdType.FIGI,
      secId = "BBG000B9XRY4",
      exchange = "SMART"
    )

  val ByIssuerId = Contract(issuerId = "e1453318")
