package org.quasigroup

import java.util

package object basic {

  final case class Position(contract: Contract, account: String, position: Int, marketPrice: Double, marketValue: Double, averageCost: Double, unrealPnl: Double, realPnl: Double)

  final case class TagValue(p_tag: String, p_value: String)

  final case class OrderState(status: String, initMargin: String, maintMargin: String, equityWithLoan: String, commission: Double, minCommission: Double, maxCommission: Double, commissionCurrency: String, warningText: String)

  final case class OrderComboLeg(p_price: Double)

  final case class ComboLeg(p_conId: Int, p_ratio: Int, p_action: String, p_exchange: String, p_openClose: Int, p_shortSaleSlot: Int, p_designatedLocation: String, p_exemptCode: Int)

  final case class Contract(p_conId: Int, p_symbol: String, p_secType: String, p_expiry: String, p_strike: Double, p_right: String, p_multiplier: String, p_exchange: String, p_currency: String, p_localSymbol: String, p_tradingClass: String, p_comboLegs: util.ArrayList[ComboLeg], p_primaryExch: String, p_includeExpired: Boolean, p_secIdType: String, p_secId: String)

  final case class DeltaNeutralContract(conid: Int, delta: Double, price: Double)

  final case class ContractDetails(p_contract: Contract, p_marketName: String, p_minTick: Double, p_orderTypes: String, p_validExchanges: String, p_underConId: Int, p_longName: String, p_contractMonth: String, p_industry: String, p_category: String, p_subcategory: String, p_timeZoneId: String, p_tradingHours: String, p_liquidHours: String, p_evRule: String, p_evMultiplier: Double)

  final case class Execution(p_orderId: Int, p_clientId: Int, p_execId: String, p_time: String, p_acctNumber: String, p_exchange: String, p_side: String, p_shares: Int, p_price: Double, p_permId: Int, p_liquidation: Int, p_cumQty: Int, p_avgPrice: Double, p_orderRef: String, p_evRule: String, p_evMultiplier: Double)

  final case class ExecutionFilter(p_clientId: Int, p_acctCode: String, p_time: String, p_symbol: String, p_secType: String, p_exchange: String, p_side: String)
}
