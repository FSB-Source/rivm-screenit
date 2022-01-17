import type {HuisartsLijstStateProps, HuisartsListItem} from "./HuisartsLijstView"
import HuisartsLijstView from "../huisarts/HuisartsLijstView"
import {connect} from "react-redux"
import type {Huisarts} from "../../../datatypes/Huisarts"
import {getHuisartsVolledigAdres} from "../../../datatypes/Huisarts"
import {RootState} from "../../../Store"
import {HuisartsZoekFilter} from "./HuisartsZoekenView"
import {Afspraak} from "../../../datatypes/Afspraak"

export type HuisartsLijstContainerProps = {
	huisartsFilter: HuisartsZoekFilter;
	afspraak: Afspraak;
	vergrendelZoekState: () => void;
	onKiesHuisarts: (huisarts: Huisarts) => void;
	toggle: () => void;
	moetVerversen: boolean;
}

const mapStateToProps = (state: RootState, ownProps: HuisartsLijstContainerProps): HuisartsLijstStateProps => {
	const huisartsItems: Array<HuisartsListItem> = []
	let huisartsenFiltered: Array<Huisarts> = Array.from(state.huisartsenById.values())

	if (ownProps.huisartsFilter.naamHuisarts !== "") {
		huisartsenFiltered = huisartsenFiltered.filter(h => h.naamHuisarts && h.naamHuisarts.toLocaleLowerCase().includes(ownProps.huisartsFilter.naamHuisarts.toLocaleLowerCase()))
	}

	if (ownProps.huisartsFilter.straatnaam !== "") {
		huisartsenFiltered = huisartsenFiltered.filter(h => h.straatnaam && h.straatnaam.toLocaleLowerCase().includes(ownProps.huisartsFilter.straatnaam.toLocaleLowerCase()))
	}

	if (ownProps.huisartsFilter.postcode !== "") {
		huisartsenFiltered = huisartsenFiltered.filter(h => h.postcode && h.postcode.toLocaleLowerCase().includes(ownProps.huisartsFilter.postcode.toLocaleLowerCase()))
	}

	if (ownProps.huisartsFilter.plaats !== "") {
		huisartsenFiltered = huisartsenFiltered.filter(h => h.plaats && h.plaats.toLocaleLowerCase().includes(ownProps.huisartsFilter.plaats.toLocaleLowerCase()))
	}

	huisartsenFiltered.map(h => huisartsItems.push({
		id: h.id,
		naam: h.naamHuisarts,
		type: h.type,
		adres: getHuisartsVolledigAdres(h),
		praktijknaam: h.praktijknaam,
	}))
	return {
		...ownProps,
		huisartsItems: huisartsItems,
		huisartsen: state.huisartsenById,
	}
}

const HuisartsLijstContainer = connect(mapStateToProps)(HuisartsLijstView)
export default HuisartsLijstContainer