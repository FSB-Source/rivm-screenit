export type TijdelijkAdres = {
	straat: string;
	huisnummer: number;
	huisletter?: string;
	huisnummerToevoeging?: string;
	huisnummerAanduiding?: string;
	postcode: string;
	plaats: string;
	startDatum?: string;
	eindDatum?: string;
};
export const getLocatie = (tijdelijkAdres: TijdelijkAdres): string => {
	return `${tijdelijkAdres.straat || ""} ${tijdelijkAdres.huisnummer || ""} ${tijdelijkAdres.huisletter || ""} ${tijdelijkAdres.huisnummerToevoeging || ""} ${tijdelijkAdres.huisnummerAanduiding || ""}`
}