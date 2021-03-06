# 91 APP DATA ANALYSIS


# FINAL REPORT
## PART1: 91APP data cleaning, data visualization
### [FinalReport_part1](https://github.com/Catherine0822/CSX4001/blob/master/FinalReport/FinalReport_part1.ipynb)
#### 1.ShoppingCarts.csv共變異數矩陣
#### 2.不同付款方式與不同使用平台，與總價格/數量的關係
#### 3.不同付款方式與不同使用裝置，與總價格/數量的關係
#### 4.總價格與總數量的關係
#### 5.ShoppingCarts.csv每月總營收
#### 6.ShoppingCarts.csv週間訂單數
#### 7.PromotionOrders.csv與TargetOrders.csv合併、清理、視覺化
#### 8.各優惠折扣歷史總使用次數
#### 9.各優惠折扣每月使用次數

## PART2: 此 Notebook 的目的在於透過顧客的交易紀錄，進而預測此顧客未來的顧客屬性。
### [FinalReport_part2](https://github.com/Catherine0822/CSX4001/blob/master/FinalReport/FinalReport_part2.ipynb)
#### 1.我們將顧客屬性分為 ABCDEF 六個等級，未來有新的一筆交易資料進來時，可以透過其歷史購買紀錄和購買種類分布，透過機器學習方式預測其未來最終可能對公司的總貢獻是屬於哪個顧客屬性。
#### 2.目前擁有的Dataset作為歷史訓練資料集，可以得到他的歷史Rank (god) 以及前N筆購買紀錄當下的Rank變動。
#### 3.當未來有一筆新的交易紀錄近來，我可以透過此顧客的歷史交易紀錄，來預測此消費者的顧客屬性，並作為後續Promotion / Ecoupon的投遞依據。

## PART3: 從第一年度的會員消費表現，預測第二年度是否會留存，以及留存的所屬類別
### [FinalReport_part3](https://github.com/Catherine0822/CSX4001/blob/master/FinalReport/FinalReport_part3.ipynb)
#### 1.我們專案的核心問題，是希望根據會員前幾筆消費行為，來預測未來的總貢獻（並據此給出一個貢獻度排名），並針對不同類型的會員給特定的promotion/ecoupon以促進消費。
#### 2.然而若直接用91app橫跨三年度的資料來做為訓練集，會忽略recency（是否最近購買）的影響，例如有兩個人同樣都被列為VIP，但第一個人只有在第一年度進行消費（後面兩年都沒有再消費），第二個人只在第三年度消費，實務上來說，明顯第二個人才是真正有貢獻者。
#### 3.因此，為了處理時間的問題，我們改成每次只考慮「一年」的會員表現（換句話說，「一年」之前同個會員的消費表現不考慮在貢獻排名裡面），並從這一年內的消費行為來預測隔年是否留存，以及留存的所屬類別。在應用上，我們就可以直接在隔年年初從預測的類別去給予特定的促銷或優惠活動。

## PART4: 分析不同的顧客分群與使用promotion之間的關係，我們預期看到在不同族群的˙顧客身上用不同的promotion會有不同效果
### [FinalReport_part4](https://catherine0822.github.io/CSX4001/FinalReport/FinalReport_part4.html)
#### 1. 透過統計方法看不同組之間有無使用promotion的meanpay有沒有差別–>即使有效果也無法說明給a組promotion比較有效因為我們不知道a組共拿了多少promotion
#### 2. 整理出每個客戶每一筆消費特徵值
#### 3. 將part2 整理出來的特質們透過降維，挑出同一組特徵相同的人，獨變向為使用不同種類的promotion(背後動機為AB test，控制掉其他變相後比較唯一有無promotion的差異)–>看之後的second/third/fourth/fifth_info兩人的升降有無差異–>進一步下結論哪一分群的客戶適合使用什麼種類的promotion

## HW4~6 : Text Mining
#### Analyzed 3860 news related to e-commerce platform
#### TF-IDF

## HW2-3 : explorational data analysis on 91APP 
### [HW3](https://catherine0822.github.io/CSX4001/HW3/HW3.html)
#### 圖1 : hr 與購買總額: 晚上11:00-1:00為購買總額巔峰時段
#### 圖2 : 星期與購買總額 : 星期六是購買總額最低
#### 圖3 : 購買天數差(每個人兩次消費之間的天數差)分布:
####      - 天數差由0天開始(即一日內消費了兩次)為高峰，之後漸漸下降至200天為止
*沒有任何天數差是介於200-450天之間
*450天之後到550天之間，又出現一群消費
		  
####    (其實原本想要用memberid做為每一筆資料，但因為天數差跟隨的是購物車編號，且同一購物車中的不同
*單品也可能用不同promotion, 因此最後的資料是以每一個單品為單位)
#### 	 note: 同一單品有可能用兩種promotion
#### 圖1: 相同購買購買天數差的消費與 左y軸:單筆消費平均購買數量 右y軸: 單筆消費平均買金額
#### 		  - 似乎看不出明顯差異，但購買總額有微微上升的趨勢
#### 圖2,3: 觀察diff<400 (G1), diff介於12~200(G2), diff>400(G3)的三種不同類別的購買與promotion的關係
#### 		  - 可以觀察到 1. diff<200的人比較多用的是任意優惠價 2.diff>400的人比較多用滿額折線與滿額贈 

#### 圖 4,5: 購買兩次以上單品的人的所有消費中，每個單品有無使用promotion與那筆消費的天數差的關係
####        (粉紅色為沒有使用promtion, 藍色為有使用promotion)
#### 可以觀察到介於diff<200使用promotion的比例較高，diff>400的單品使用的比例反而較低
#### 拆成diff <12 , 12~diff~200 , diff>400三組來看的話可以看出12~diff~200是使用promotion比例最高的

## HW1 : Exploratory Data Analysis
#### 對野村CRM資料寄行初步EDA
