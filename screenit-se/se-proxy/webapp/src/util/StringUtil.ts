export const ucf = (s: string): string => s.length < 1 ? s : s[0].toUpperCase() + s.slice(1)
export const annotatieTitle = (s: string): string => {
	return ucf(s.toLowerCase().replace("signalering_", "").replace("legacy_", "").split("_").join(" "))
}

export const zelfdeTekst = (tekst: string | undefined, vergelijkTekst: string | undefined): boolean => {
	return tekst === vergelijkTekst || (!tekst && !vergelijkTekst)
}