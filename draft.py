
pip install PyAutoIt
pip install tushare --upgrade

import tushare as ts
df = ts.get_hist_data('000875')
