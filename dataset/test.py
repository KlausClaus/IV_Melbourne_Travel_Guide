import pandas as pd

import pandas as pd

# 从car_parks.csv中读取数据
df = pd.read_csv('car_parks.csv')

# 清理parking_spaces小于1的行
df_cleaned = df[df['parking_spaces'] >= 100]

# 保存清理后的数据到CSV文件
df_cleaned.to_csv('cleaned_car_parks.csv', index=False)
