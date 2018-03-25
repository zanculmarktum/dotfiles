# Uncomment this to still load settings configured via autoconfig.yml
# config.load_autoconfig()

c.backend = 'webkit'
c.content.developer_extras = True
c.url.searchengines = {'DEFAULT': 'https://duckduckgo.com/?q={}', 'g': 'https://www.google.com/search?q={}'}
c.fonts.web.family.fixed = '"DejaVu Sans Mono"'
c.fonts.web.family.sans_serif = 'Cantarell'
c.fonts.web.family.serif = 'Cantarell'
c.fonts.web.family.standard = None
c.fonts.web.size.default = 12
c.fonts.web.size.default_fixed = 12
c.fonts.web.size.minimum = 12
c.url.default_page = 'about:blank'
c.url.start_pages = c.url.default_page
