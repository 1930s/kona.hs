# Kona

```
kona – A Crawler for Konachan.com

Usage: kona TAGS... [-r|--rating RATING] [-k|--kind KIND]
            [-w|--max_worker MAX_WORKER] [-o|--output OUTPUT] [-d|--delay DELAY]
            [--retries RETRIES]
  Download all images of TAGS

Available options:
  -r,--rating RATING       Hentainess of the images. Choose from: safe,
                           questionable, explict, questionableminus,
                           questionableplus (default: "safe")
  -k,--kind KIND           Kind of image to download. Choose from: preview,
                           sample, origin (default: "origin")
  -w,--max_worker MAX_WORKER
                           Limit numbers of task in parallel (default: 16)
  -o,--output OUTPUT       Output path (default: "images")
  -d,--delay DELAY         Start next try after DELAY ms (default: 100)
  --retries RETRIES        Retry RETRIES times, otherwise the program will
                           fail (default: 5)
  -h,--help                Show this help text
```
