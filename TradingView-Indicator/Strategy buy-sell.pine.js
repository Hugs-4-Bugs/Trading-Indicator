// This source code is subject to the terms of the Mozilla Public License 2.0 at https://mozilla.org/MPL/2.0/
// © Hexa

//@version=4
study("Strategy buy-sell", overlay=true)

//==================== INDICATEUR DE BASE POUR LES TENDANCES ====================\\
Periode =  input(title="ATR Length", defval=10)
AvgTrueRange = rma(tr, Periode)
Src = input(hl2, title="source")
Multiplier = input(title="ATR Multiplier", type=input.float, step=0.1, defval=3.0)
UpTrend = Src-(Multiplier*AvgTrueRange)
UpTrend1 = nz(UpTrend[1],UpTrend)
UpTrend := close[1] > UpTrend1 ? max(UpTrend,UpTrend1) : UpTrend

//==================== SELECTEUR D'UNITE DE TEMPS  ====================\\
mode = input(title = "Multi time Mode", defval = 'Off', options=['Off', 'On'])
SignalMode = input(title = "Signal Mode", defval = 'Off', options=['Off', 'On'])
isHA = input(title="Scalping mode",defval = 'Off', options=['Off', 'On'])
HTFm = input('5', title = "Tendance unité de temps longue", type=input.resolution)
HTFm1 = input('5', title = "Tendance unité de temps courte", type=input.resolution)

//Multi time mode
HTFo = timeframe.period == '1' ? '5' :
  timeframe.period == '3' ? '15' : 
  timeframe.period == '5' ? '15' : 
  timeframe.period == '15' ? '60' : 
  timeframe.period == '30' ? '60' : 
  timeframe.period == '45' ? '60' : 
  timeframe.period == '60' ? '240' : 
  timeframe.period == '120' ? '240' : 
  timeframe.period == '180' ? '240' : 
  timeframe.period == '240' ? 'D' : 
  timeframe.period == 'D' ? 'W' :
  '5W'

HTF = mode == 'Off' ? HTFo : HTFm
HTF1 = mode == 'Off' ? HTFo : HTFm1

//==================== TENDANCE DE L'UNITE DE TEMPS COURTE  ====================\\
PeriodeUTC = security(syminfo.tickerid,HTF1,Periode)
AvgTrueRangeUTC = security(syminfo.tickerid,HTF1,AvgTrueRange)
SrcUTC = security(syminfo.tickerid,HTF1,Src)
MultiplierUTC = security(syminfo.tickerid,HTF1,Multiplier)
CloseUTC = security(syminfo.tickerid,HTF1,close)

//Calcul de la tendance haussière sur UTC
UpTrendUTC = SrcUTC-(MultiplierUTC*AvgTrueRangeUTC)
UpTrendUTC1 = nz(UpTrendUTC[1],UpTrendUTC)
UpTrendUTC := CloseUTC[1] > UpTrendUTC1 ? max(UpTrendUTC,UpTrendUTC1) : UpTrendUTC

//Calcul de la tendance baissière sur UTC
DnTrendUTC = SrcUTC+(MultiplierUTC*AvgTrueRangeUTC)
DnTrendUTC1 = nz(DnTrendUTC[1],DnTrendUTC)
DnTrendUTC := CloseUTC[1] < DnTrendUTC1 ? min(DnTrendUTC,DnTrendUTC1) : DnTrendUTC

//Calcul du changement de tendance sur UTC
trendUTC = 1
trendUTC := nz(trendUTC[1], trendUTC)
trendUTC := trendUTC == -1 and close > DnTrendUTC1 ? 1 : trendUTC == 1 and close < UpTrendUTC1 ? -1 : trendUTC

upPlot1 = plot(trendUTC == 1 ? UpTrendUTC : na, title="Tendance haussière unité de temps courte", style=plot.style_linebr, linewidth=2, color=color.green, transp=50)
dnPlot1 = plot(trendUTC == 1 ? na : DnTrendUTC, title="Tendance baissière unité de temps courte", style=plot.style_linebr, linewidth=2, color=color.red, transp=50)

//==================== TENDANCE DE L'UNITE DE TEMPS LONGUE  ====================\\
PeriodeUTL = security(syminfo.tickerid,HTF,Periode)
AvgTrueRangeUTL = security(syminfo.tickerid,HTF,AvgTrueRange)
SrcUTL = security(syminfo.tickerid,HTF,Src)
MultiplierUTL = security(syminfo.tickerid,HTF,Multiplier)
CloseUTL = security(syminfo.tickerid,HTF,close)

//Calcul de la tendance haussière sur UTL
UpTrendUTL = SrcUTL-(MultiplierUTL*AvgTrueRangeUTL)
UpTrendUTL1 = nz(UpTrendUTL[1],UpTrendUTL)
UpTrendUTL := CloseUTL[1] > UpTrendUTL1 ? max(UpTrendUTL,UpTrendUTL1) : UpTrendUTL

//Calcul de la tendance baissière sur UTL
DnTrendUTL = SrcUTL+(MultiplierUTL*AvgTrueRangeUTL)
DnTrendUTL1 = nz(DnTrendUTL[1],DnTrendUTL)
DnTrendUTL := CloseUTL[1] < DnTrendUTL1 ? min(DnTrendUTL,DnTrendUTL1) : DnTrendUTL

//Calcul du changement de tendance sur UTL
trendUTL = 1
trendUTL := nz(trendUTL[1], trendUTL)
trendUTL := trendUTL == -1 and close > DnTrendUTL1 ? 1 : trendUTL == 1 and close < UpTrendUTL1 ? -1 : trendUTL

upPlot = plot(trendUTL == 1 ? UpTrendUTL : na, title="Tendance haussière unité de temps longue", style=plot.style_linebr, linewidth=2, color=color.green)
dnPlot = plot(trendUTL == 1 ? na : DnTrendUTL, title="Tendance baissière unité de temps longue", style=plot.style_linebr, linewidth=2, color=color.red)

//Remplissage des tendances
//Hausse
fill(upPlot1, upPlot, title="Nuage haussier", color=color.green, transp=90)
//Baisse
fill(dnPlot1, dnPlot, title="Nuage baissier", color=color.red, transp=90)
//Range
fill(dnPlot1, upPlot, title="Nuage range", color=color.white, transp=90)
fill(upPlot1, dnPlot, title="Nuage range", color=color.white, transp=90)

//==================== FILTRE DE TENDANCE AVEC UTC & UTL + SIGNAL ENTREE & SORTIE ====================\\
//Déclaration des variables pour régler les périodes de calculs du stochastique
periodK = input(20, title="K", minval=1)
periodD = input(5, title="D", minval=1)
smoothK = input(5, title="Smooth", minval=1)

//Déclaration des fonctions du stochastique
k = sma(stoch(close, high, low, periodK), smoothK)
d = sma(k, periodD)

//Dessin pour le stochastique
//plot(k, title="%K", color=#0094FF)
//plot(d, title="%D", color=#FF6A00)

//Signal d'achat uniquement en tendance haussière
UpSensibility = input(30,title="Sensibilité de la zone d'achat", maxval=49)
crossSup = k>d and k[1]<d[1] //Fonction pour déterminer le croissement de k et d
BuySignal = k<UpSensibility and crossSup //Signal d'achat
Buy = iff(trendUTL>0 and trendUTC>0,BuySignal,na) //Tendance haussière
GetArroundBuy = iff(Buy,high,na) //Fonction pour contourner le problème d'affichage
OnOffBuy = iff(SignalMode == 'On',GetArroundBuy,na) //On/Off Affichage Signaux acheteur
plotshape(OnOffBuy, title="Buy", text="Buy", location=location.absolute, style=shape.labeldown, size=size.tiny, color=color.green, textcolor=color.white, transp=0)

//Signal de vente uniquement en tendance baissière
DnSensibility = input(70,title="Sensibilité de la zone de vente", minval=51)
crossInf = k<d and k[1]>d[1] //Fonction pour déterminer le croissement de k et d
SellSignal = k>DnSensibility and crossInf //Signal de vente
Sell = iff(trendUTL<1 and trendUTC<1,SellSignal,na) //Tendance baissière
GetArroundSell = iff(Sell,high,na) //Fonction pour contourner le problème d'affichage
OnOffSell= iff(SignalMode == 'On',GetArroundSell,na) //On/Off Affichage Signaux vendeur
plotshape(OnOffSell, title="Sell", text="Sell", location=location.absolute, style=shape.labeldown, size=size.tiny, color=color.red, textcolor=color.white, transp=0)

barcolor(OnOffSell ? color.white :OnOffBuy ? color.white:na)//Fonction pour colorer les chandeliers lors d'un signal d'achat ou de vente

//==================== STOP LOSS ====================\\
//Indicateur ATR pour le Stop loss (utilisation des lignes 8 à 14)
ATRStopLossUpTrend = Src-(Multiplier*AvgTrueRange) //Stop loss quand signal haussier
ATRStopLossDnTrend = Src+(Multiplier*AvgTrueRange) //Stop loss quand signal baissier
StopLoss = iff(GetArroundBuy,ATRStopLossUpTrend,iff(GetArroundSell,ATRStopLossDnTrend,na)) //Formule pour dessiner les stop loss uniquement au moment des signaux d'achats ou de ventes
OnOffStopLoss = iff(SignalMode == 'On',StopLoss,na) //On/Off Affichage Stop Loss
plot(OnOffStopLoss, title="Stop Loss",  style=plot.style_linebr, linewidth=4, color=color.red)

//==================== SCALPING SIGNAL ====================\\
//Indicateur de signal scalping en Heiken Ashi
tickerid = tickerid(syminfo.prefix, syminfo.ticker, session.regular, adjustment.splits)
data = isHA == 'On' ? heikinashi(tickerid) : tickerid
o = iff(isHA == 'On',security(data, timeframe.period, open),na)
h = iff(isHA == 'On',security(data, timeframe.period, high),na)
l = iff(isHA == 'On',security(data, timeframe.period, low),na)
c = iff(isHA == 'On',security(data, timeframe.period, close),na)
col = c > o ? color.green : color.red
plotcandle(o,h,l,c, "Heiken Ashi", col, color.gray, bordercolor=color.gray)

//Signal scalping d'achat uniquement en tendance haussière
BuySignalScalp = c > o and c[1] < o[1] and c[2] < o[2] and c[3] < o[3]
BuyScalp = iff(trendUTL>0 and trendUTC>0,BuySignalScalp,na) //Tendance haussière
GetArroundBuyScalp = iff(BuyScalp,high,na) //Fonction pour contourner le problème d'affichage
OnOffBuyScalp = iff(isHA == 'On',GetArroundBuyScalp,na) //On/Off Affichage signaux du Mode Scalping
plotshape(OnOffBuyScalp, title="Buy Scalp", text="Buy Scalp", location=location.absolute, style=shape.labeldown, size=size.tiny, color=color.green, textcolor=color.white, transp=0)

//Signal scalping de vente uniquement en tendance baissière
SellSignalScalp = c < o and c[1] > o[1] and c[2] > o[2] and c[3] > o[3]
SellScalp = iff(trendUTL<1 and trendUTC<1,SellSignalScalp,na) //Tendance baissière
GetArroundSellScalp = iff(SellScalp,high,na) //Fonction pour contourner le problème d'affichage
OnOffSellScalp = iff(isHA == 'On',GetArroundSellScalp,na) //On/Off Affichage signaux du Mode Scalping
plotshape(OnOffSellScalp, title="Sell Scalp", text="Sell Scalp", location=location.absolute, style=shape.labeldown, size=size.tiny, color=color.red, textcolor=color.white, transp=0)

//==================== MONEY MANAGEMENT ====================\\
//Calcul de la quantité à acheter en fonction du risque entré par l'utilisateur
//Risque = input(20,title="Risque (ex : 100$)")
//DistSLSell = ATRStopLossDnTrend - SellSignal
//DistSLBuy = BuySignal - ATRStopLossUpTrend
//Quantite = iff(Sell,Risque/DistSLSell,iff(Buy,Risque/DistSLBuy,na))
//DistFinal = iff(Sell,DistSLSell,iff(Buy,DistSLBuy,na))
//nl = "\n"
//string MM = Quantite + nl + DistFinal 

//OffSetLabel = input(5,"Label Stop loss et quantité")
//OffSet = OffSetLabel * (time-time[1])
//TextStopLoss = "Distance Stop loss : "
//TextQuantite = "Quantité d'actif : "
//nl = "\n"
//string TexteDynamique = TextStopLoss + nl + TextQuantite
//var label id = na
//label.delete(id)
//id := label.new(x=time+OffSet, y=high, xloc=xloc.bar_time, text=MM)

//==================== ALERTE ====================\\
















