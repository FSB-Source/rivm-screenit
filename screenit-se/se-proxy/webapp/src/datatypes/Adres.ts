export type Adres = {
	locatieBeschrijving: string;
	postcode: string;
	plaats: string;
};

export const postcodeMetSpatie = (postcode: string): string => {
	return postcode ? `${postcode.substring(0, 4)} ${postcode.substring(4, 6)}` : ""
}