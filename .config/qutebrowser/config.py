# Uncomment this to still load settings configured via autoconfig.yml
# config.load_autoconfig()

config.source('nord-qutebrowser/nord-qutebrowser.py')

#c.backend = 'webkit'
c.url.searchengines = {
    'DEFAULT': 'https://startpage.com/do/search?query={}',
    'd': 'https://duckduckgo.com/?q={}',
    'x': 'https://www.searx.me/?q={}',
    's': 'https://saucenao.com/search.php?url={}',
    'i': 'https://invidio.us/search?q={}',
    'yt': 'https://www.youtube.com/results?search_query={}',
    'y': 'https://yandex.com/search/?text={}',
}
c.fonts.web.family.fixed = '"DejaVu Sans Mono"'
c.fonts.web.family.sans_serif = 'Cantarell'
c.fonts.web.family.serif = 'Cantarell'
c.fonts.web.family.standard = 'Cantarell'
c.fonts.web.size.default = 12
c.fonts.web.size.default_fixed = 12
c.fonts.web.size.minimum = 12
c.url.default_page = 'about:blank'
c.url.start_pages = c.url.default_page
