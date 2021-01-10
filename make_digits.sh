mkdir -p digits

# Preview fonts
# ls /nix/store/p5zrn0kvb1vw91bb06jzmm2d4dczy7d7-toilet-0.3/share/figlet | xargs -I {} sh -c 'echo {}; toilet -f {} -W 514'

function digit() {
  toilet -f mono9 -W
 # | head -n -2
}

echo 1 | digit > digits/1
echo 2 | digit > digits/2
echo 3 | digit > digits/3
echo 4 | digit > digits/4
echo 5 | digit > digits/5
echo 6 | digit > digits/6
echo 7 | digit > digits/7
echo 8 | digit > digits/8
echo 9 | digit > digits/9
echo 0 | digit > digits/0
