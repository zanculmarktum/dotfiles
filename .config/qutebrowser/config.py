# Uncomment this to still load settings configured via autoconfig.yml
# config.load_autoconfig()

config.source('nord-qutebrowser/nord-qutebrowser.py')

#c.backend = 'webkit'
c.url.searchengines = {'DEFAULT': 'https://www.searx.me/?q={}', 'd': 'https://duckduckgo.com/?q={}', 's': 'https://startpage.com/do/search?query={}'}
c.fonts.web.family.fixed = '"DejaVu Sans Mono"'
c.fonts.web.family.sans_serif = 'Cantarell'
c.fonts.web.family.serif = 'Cantarell'
c.fonts.web.family.standard = 'Cantarell'
c.fonts.web.size.default = 12
c.fonts.web.size.default_fixed = 12
c.fonts.web.size.minimum = 12
c.url.default_page = 'about:blank'
c.url.start_pages = c.url.default_page
