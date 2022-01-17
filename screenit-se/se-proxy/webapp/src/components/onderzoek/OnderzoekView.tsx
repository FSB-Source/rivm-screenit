import React, {Component} from "react"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {SubPagina} from "../../datatypes/Navigation"
import DaglijstView from "../daglijst/DaglijstView"
import VorigeOnderzoekenContainer from "./VorigeOnderzoekenContainer"
import VisueleInspectieContainer from "./VisueleInspectieContainer"
import SignalerenContainer from "./SignalerenContainer"
import {sendEmptyStudyMessageToIMS, sendStudyMessageToIMS} from "../../util/ImsApiUtil"
import type {Client} from "../../datatypes/Client"
import {store} from "../../Store"
import {persistentErrorToast} from "../../util/ToastUtil"
import {navigateToDaglijst} from "../../util/NavigationUtil"

export type OnderzoekViewProps = {
	client: Client;
	afspraak: Afspraak;
	subPagina?: SubPagina;
	gebruikersnaam?: string;
	studyForIms: number;
	activeStudyForIms?: number;
};

export type OnderzoekViewState = {
	heeftOudeBeeldenOpgevraagd: boolean;
};

export default class OnderzoekView extends Component<OnderzoekViewProps, OnderzoekViewState> {

	constructor(props: OnderzoekViewProps) {
		super(props)
		this.state = {
			heeftOudeBeeldenOpgevraagd: false,
		}
		this.setHeeftOudeBeeldenOpgevraagd = this.setHeeftOudeBeeldenOpgevraagd.bind(this)
	}

	toonBeeldenOpBekijkStation(): void {
		if (this.props.gebruikersnaam) {
			if (this.props.studyForIms && this.props.activeStudyForIms !== this.props.studyForIms) {
				sendStudyMessageToIMS(this.props.studyForIms, this.props.client.bsn, this.props.gebruikersnaam)
			} else if (!this.props.studyForIms) {
				sendEmptyStudyMessageToIMS(this.props.gebruikersnaam)
			}
		}
	}

	componentDidMount(): void {
		this.toonBeeldenOpBekijkStation()
	}

	setHeeftOudeBeeldenOpgevraagd = (): void => {
		this.setState({
			heeftOudeBeeldenOpgevraagd: true,
		})
	}

	componentDidUpdate(prevProps: OnderzoekViewProps): void {
		if ((prevProps.subPagina === "Visuele inspectie" && this.props.subPagina === "Signaleren")
			|| (prevProps.subPagina === "Signaleren" && this.props.subPagina === "Visuele inspectie")
			|| (this.state.heeftOudeBeeldenOpgevraagd && prevProps.subPagina !== this.props.subPagina)) {
			this.toonBeeldenOpBekijkStation()
		}
	}

	componentWillUnmount(): void {
		if (store.getState().session && this.props.gebruikersnaam) {
			sendEmptyStudyMessageToIMS(this.props.gebruikersnaam)
		}
	}

	componentDidCatch(): void {
		persistentErrorToast("Er ging iets fout in de applicatie, probeer het opnieuw of neem contact op met een beheerder")
		navigateToDaglijst(store.dispatch)
	}

	render(): JSX.Element {
		switch (this.props.subPagina) {
			case "Vorige onderzoeken":
				return <div className="onderzoek-scherm vorige-onderzoeken">
					<VorigeOnderzoekenContainer
						client={this.props.client} afspraak={this.props.afspraak} gebruikersnaam={this.props.gebruikersnaam}
						setHeeftOudeBeeldenOpgevraagd={this.setHeeftOudeBeeldenOpgevraagd}/>
				</div>
			case "Visuele inspectie":
				return <div className="onderzoek-scherm">
					<VisueleInspectieContainer
						client={this.props.client}
						afspraak={this.props.afspraak}/>
				</div>
			case "Signaleren":
				return <div className="onderzoek-scherm">
					<SignalerenContainer
						client={this.props.client}
						afspraak={this.props.afspraak}/>
				</div>
			default:
				return <DaglijstView/>
		}
	}

}
