#!/usr/bin/ruby

require 'nokogiri'
require 'open-uri'

def get_page(url)
	page = Nokogiri::parse(open(url).read)

	page.search('.membersListPage-userName > a').each do |user|
		user = user.attributes['href'].to_s.scan(/\/([^\/]+)$/)[0][0]
		profile = Nokogiri::parse(open("http://ripplefederation.org/Members/#{user}").read)
		address = profile.search('.profileCoverArea-questionAndAnswer').map do |qa|
			a = qa.text.scan(/Ripple Address:.*?(r[rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz]*)\s*$/m)
			a[0][0] if a && a[0]
		end.compact.first

		if address
			puts "INSERT INTO aliases (alias,domain,ripple) VALUES('#{user}', 'ripplefederation.org', '#{address}');"
			STDOUT.flush
		end
	end

	nxt = page.at('a.pagination-next')
	if nxt
		get_page(nxt.attributes['href'].to_s)
	end
end

get_page('http://ripplefederation.org/Members?sort=alphabetical')
