# FXCM 量化对冲实时数据

---

[<img src='文艺坊图库/RStudioCloud.png' height='20'>](https://rstudio.cloud) [<img src='文艺坊图库/RStudioCom2.png' height='20'>](https://community.rstudio.com/new-topic?category=shiny&tags=shiny) [![](文艺坊图库/shiny-badge.svg)](https://www.shinyapps.io)

**大秦赋 (Chinese Emperor)**<br>
春秋战国《*礼记•经解*》<br>
孔子曰：『君子慎始，差若毫厘，缪以千里。』

- ticked data：委托挂单数据（毫秒计：差之毫厘，失之千里）
- ticked data price：委托挂单报价（毫秒计：差之毫厘，失之千里）
- [什么是Tick 数据？](https://blog.csdn.net/weixin_42219751/article/details/98870005)

## 1. 采实时日内汇价

- **FXCM每周委托挂单数据**：点击[FXCMTickData](https://github.com/FXCMAPI/FXCMTickData) 获取历史委托挂单汇价（汇价数据默认时间为🇬🇧`GMT+0`）。
- **Historical Data Downloader Basic** : 点击[Historical Spreads](https://www.fxcm.com/uk/why-fxcm/execution/historical-spreads)获取历史汇价数据。

此外，也可点击[**DataCollection**](https://beta.rstudioconnect.com/content/3153)获取历史汇价数据，回测并筛选最优统计模型，再进行交易。

<img src='文艺坊图库/ice_video_20171113-013636.gif' width='240'>

## 2. 高频量化自动演算交易

采集日内汇价...
  
<img src='文艺坊图库/under_construction.jpg' width='240'>

## 3. 参考资源

01. [binary.com : Job Application - Quantitative Analyst](https://github.com/englianhu/binary.com-interview-question) ❤️‍🔥
02. [如何用R语言开始量化投资](https://github.com/scibrokes/real-time-fxcm/blob/master/reference/%E5%A6%82%E4%BD%95%E7%94%A8R%E8%AF%AD%E8%A8%80%E5%BC%80%E5%A7%8B%E9%87%8F%E5%8C%96%E6%8A%95%E8%B5%84.pdf)
03. [解密复兴科技 - 基于隐蔽马尔科夫模型的时序分析方法](https://github.com/scibrokes/real-time-fxcm/blob/master/reference/%E8%A7%A3%E5%AF%86%E5%A4%8D%E5%85%B4%E7%A7%91%E6%8A%80%20-%20%E5%9F%BA%E4%BA%8E%E9%9A%90%E8%94%BD%E9%A9%AC%E5%B0%94%E7%A7%91%E5%A4%AB%E6%A8%A1%E5%9E%8B%E7%9A%84%E6%97%B6%E5%BA%8F%E5%88%86%E6%9E%90%E6%96%B9%E6%B3%95.pdf) ❤️‍🔥
04. [原则 - 雷·达里奥](https://github.com/scibrokes/analyse-the-finance-and-stocks-price-of-bookmakers/blob/master/reference/%E5%8E%9F%E5%88%99%20-%20%E9%9B%B7%C2%B7%E8%BE%BE%E9%87%8C%E5%A5%A5.pdf)
05. [Successful Algorithmic Trading](https://github.com/englianhu/binary.com-interview-question/blob/master/reference/Successful%20Algorithmic%20Trading.pdf)
06. [量化交易学习-订单簿建模](https://zhuanlan.zhihu.com/p/499342831)
07. [哪种量化策略在国内最有前途？](https://www.zhihu.com/question/68030592/answer/2239306330)
08. [知呼：量化交易的频谱分析与策略搭配规律](https://zhuanlan.zhihu.com/p/89404944) ❤️‍🔥
09. [知呼：量化投资方面，国内外都有哪些好的论坛或者网站？](https://www.zhihu.com/question/20874888/answer/61854182) ❤️‍🔥
10. [韦纳软件 VeighNa](https://www.vnpy.com) ❤️‍🔥
11. [GitHub : VeighNa - By Traders, For Traders.](https://github.com/vnpy/vnpy) ❤️‍🔥
12. [知乎：最新33家国内百亿量化私募 Hedge Fund 核心人员背景简析（含千亿级）20220226](https://zhuanlan.zhihu.com/p/288461500)
13. [知乎：中国量化私募：竞争全球，背水一战！](https://zhuanlan.zhihu.com/p/145113688)
14. [GitHub : Tushare](https://github.com/waditu/tushare)
15. [Tushare Pro版](https://tushare.pro)
16. [Tushare](http://tushare.org)
17. [中国期货市场量化交易(R与C++版) - 相关推荐](https://ebook365.org/B07MVZ4XP7)
18. [金工量化优质书单推荐及下载](https://www.cxybb.com/article/m0_37639589/90765551)
19. [近十年量化交易领域最重要的十本参考书是哪些？](https://www.zhihu.com/question/23857983)
20. [知乎：量化一般用什么软件比较好，在哪里下载，还有一般量化的平台都有哪些呀？](https://www.zhihu.com/question/62413612)
21. [知乎：量化投资学习推荐的书籍都有哪些？](https://www.zhihu.com/question/54727745)
22. [揭秘全球量化投资之父詹姆斯·西蒙斯](https://open.toutiao.com/a7151346713263948325/?a_t=AHzmbrJ6CcFZ5Bb6e4ZHTBgS2B6kq493SPGCZe4PHKXZiDXJKraeg2A11zZZAtmTmUU2Crwk3&biz_log=B3fR4jutc96MzeDHNcmYzb7iUUzu2LBAjxBoiZrkX9i6umjKEKS1en7BdSPgmUjkZvFKq82g2wFtYxETJbGxufL&crypt=7198&device_brand=&dt=Redmi+7A&gy=cc1072953bd514b388e5175566825bac10a66f494272f91362dd82514b21141646040422457cc5f6e8f4ecfc878c2b5b96c47f93f42581c604dd05a3106057ee556885deee282f85349574cad79201da30d1b7d812a41fc14b0407c9d3df3eea652a365c00132c62b745a6666a271c4587d61f1eb6aa9bddafe4d34899df6a5c&item_id=7151346713263948325&label=ovm_tab_default_content_feed_1_5_v1&req_id=20221226203945D255F0A510D94B271843&utm_campaign=open&utm_medium=webview&utm_source=mi_llq_api&docid=7151346713263948325&cp=cn-toutiao&itemtype=news&version=2&mibusinessId=miuibrowser&env=production&category=news_finance&cateCode=%E8%B4%A2%E7%BB%8F)

<br>

---

[<img src="文艺坊图库/Scibrokes.png" height="14"/> Sςιβrοκεrs Trαdιηg®](http://www.scibrokes.com)<br>
<span style='color:RoyalBlue'>**[<img src="文艺坊图库/Scibrokes.png" height="14"/> 世博量化®](http://www.scibrokes.com)企业知识产权及版权所有，盗版必究。**</span>
