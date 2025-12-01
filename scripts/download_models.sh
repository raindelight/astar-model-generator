rm -rf models
mkdir  models
cd models


wget https://www.minizinc.org/challenge/2024/mznc2024_probs.tar.gz
wget https://www.minizinc.org/challenge/2023/mznc2023_probs.tar.gz
wget https://www.minizinc.org/challenge/2022/mznc2022_probs.tar.gz
wget https://www.minizinc.org/challenge/2021/mznc2021_probs.tar.gz
wget https://www.minizinc.org/challenge/2020/mznc2020_probs.tar.gz
wget https://www.minizinc.org/challenge/2019/mznc2019_probs.tar.gz
wget https://www.minizinc.org/challenge/2018/mznc2018_probs.tar.gz
wget https://www.minizinc.org/challenge/2017/mznc2017_probs.tar.gz
wget https://www.minizinc.org/challenge/2016/mznc2016_probs.tar.gz
wget https://www.minizinc.org/challenge/2015/mznc2015_probs.tar.gz
wget https://www.minizinc.org/challenge/2014/mznc2014_probs.tar.gz
wget https://www.minizinc.org/challenge/2013/mznc2013_probs.tar.gz

gzip -d mznc*


for f in *.tar; do tar xf "$f"; done
rm -rf *.tar

mv mznc2014_problems/ mznc2014-probs/
mv mznc13-problems/ mznc2013-probs/
