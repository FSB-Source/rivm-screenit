import React from "react"
import AutorisatieButton from "../generic/AutorisatieButton"
import {Col, Row} from "reactstrap"
import PaneelNaam from "../generic/PaneelNaam"
import Paneel from "../generic/Paneel"
import {store} from "../../Store"
import {BEEINDIG_KWALITEITSOPNAME, START_KWALITEITSOPNAME} from "../../actions/KwaliteitsopnameOrmActions"
import {getActieveKwaliteitsopname} from "../../restclient/WerklijstRestclient"
import {createStartBezigMetKwaliteitsopnameAction} from "../../actions/BezigMetKwaliteitsopnameActions"
import {parseKwaliteitsopnameVolgNr} from "../../util/KwaliteitsopnameUtil"

export type KwaliteitsopnameViewProps = {
	onKwaliteitsopnameAction: (actionType: string, reden: KwaliteitsopnameReden, voorOfNaKalibratie: VoorOfNaKalibratie) => void;
};

export type KwaliteitsopnameReden =
	"Periodieke kalibratie"
	| "Regulier Onderhoud"
	| "Verplaatsing"
	| "Storing"
	| "Reparatie"
	| "Vervanging rontgenbuis"
	| "Vervanging detector (LRCB)"
	| "Foutieve opname in set"
	| "Zichtbare Beeldverstoring"
	| "Error tijdens kalibratie"
	| "Graag SE bellen"
	| "Op verzoek LRCB"
	| "Kalibratie op verzoek LRCB"
	| "Nieuw mammografiesysteem (LRCB)";

const kwaliteitsopnameRedenen: Array<KwaliteitsopnameReden> = ["Periodieke kalibratie", "Regulier Onderhoud", "Verplaatsing", "Storing", "Reparatie", "Vervanging rontgenbuis", "Vervanging detector (LRCB)", "Foutieve opname in set", "Zichtbare Beeldverstoring", "Error tijdens kalibratie", "Graag SE bellen", "Op verzoek LRCB", "Kalibratie op verzoek LRCB", "Nieuw mammografiesysteem (LRCB)"]

export type VoorOfNaKalibratie = "Voor kalibratie" | "Na kalibratie" | "Geen kalibratie";
const voorOfNaKalibraties: Array<VoorOfNaKalibratie> = ["Voor kalibratie", "Na kalibratie", "Geen kalibratie"]
export type KwaliteitsopnameState = {
	reden?: KwaliteitsopnameReden;
	voorOfNaKalibratie?: VoorOfNaKalibratie;
	bezigMetKwaliteitsopname: boolean;
};

export default class KwaliteitsopnameView extends React.Component<KwaliteitsopnameViewProps, KwaliteitsopnameState> {

	constructor(props: KwaliteitsopnameViewProps) {
		super(props)
		this.state = {
			reden: undefined,
			voorOfNaKalibratie: undefined,
			bezigMetKwaliteitsopname: false,
		}
	}

	componentDidMount(): void {
		getActieveKwaliteitsopname().then(kwaliteitsopname => {
			this.setState({
				reden: kwaliteitsopname.reden,
				voorOfNaKalibratie: kwaliteitsopname.voorOfNaKalibratie,
				bezigMetKwaliteitsopname: !!(kwaliteitsopname.accessionNumber),
			})

			const accessionNumber = kwaliteitsopname.accessionNumber
			if (accessionNumber) {
				store.dispatch(createStartBezigMetKwaliteitsopnameAction(parseKwaliteitsopnameVolgNr(accessionNumber)))
			}
		})
	}

	setReden(reden: KwaliteitsopnameReden): void {
		this.setState({
			...this.state,
			...{
				reden,
			},
		})
	}

	setVoorOfNaKalibratie(voorOfNaKalibratie: VoorOfNaKalibratie): void {
		this.setState({
			...this.state,
			...{
				voorOfNaKalibratie,
			},
		})
	}

	render(): JSX.Element {
		const state = this.state
		const startEnabled = state.reden && state.voorOfNaKalibratie && !state.bezigMetKwaliteitsopname
		const stopEnabled = state.bezigMetKwaliteitsopname
		return <div className="onderzoek-scherm">
			<div className="tabpagina">
				<Row>
					<div className="onderzoek-heading">
						<h1 className="float-left">Kwaliteitsopname</h1>
					</div>
				</Row>
				<Row>
					<Col md={4}>
						<Paneel key="redenPaneel" className="onderzoek-component">
							<PaneelNaam titel={"Reden"}/>
							{kwaliteitsopnameRedenen.map(reden => {
								const actief = reden === this.state.reden
								return <Row key={reden}>
									<label className={"btn"}>
										<input type={"radio"} checked={actief} name={"reden"} disabled={stopEnabled}
											   onClick={(): void => {
												   this.setReden(reden)
											   }}/> {reden}</label>
								</Row>
							})}
						</Paneel>
					</Col>
					<Col md={4}>
						<Paneel key="kalibratiePaneel" className="onderzoek-component">
							<PaneelNaam titel={"Kalibratie"}/>
							{voorOfNaKalibraties.map(voorOfNa => {
								const actief = voorOfNa === this.state.voorOfNaKalibratie
								return <Row key={voorOfNa}>
									<label className={"btn"}>
										<input type={"radio"} checked={actief} name={"voorOfNaKalibratie"} disabled={stopEnabled}
											   onClick={(): void => {
												   this.setVoorOfNaKalibratie(voorOfNa)
											   }}/> {voorOfNa}</label>
								</Row>
							})}
						</Paneel>
					</Col>
					<Col md={4}>
						<Paneel key="werklijstPaneel" className="onderzoek-component">
							<PaneelNaam titel={"Werklijst mammograaf"}/>
							<Row>
								<AutorisatieButton
									id={"startButton"} label={"Start kwaliteitsopname"}
									heeftRecht={!!startEnabled} rechtNaam={"kwaliteitsopname"}
									popovertekst={state.bezigMetKwaliteitsopname ? "Beëindig eerst de lopende kwaliteitsopname" : !state.reden ? "Kies eerst een reden" : !state.voorOfNaKalibratie ? "Kies eerst voor/na/geen kalibratie" : undefined}
									onClick={(): void => {
										if (startEnabled) {
											const reden = state.reden ? state.reden : "Regulier Onderhoud"
											const voorOfNaKalibratie = state.voorOfNaKalibratie ? state.voorOfNaKalibratie : "Voor kalibratie"
											this.setState({
												...state,
												...{
													bezigMetKwaliteitsopname: true,
												},
											})
											this.props.onKwaliteitsopnameAction(START_KWALITEITSOPNAME, reden, voorOfNaKalibratie)
										}
									}}/>
							</Row>
							<Row>
								<label/>
							</Row>
							<Row>
								<AutorisatieButton
									id={"stopButton"} label={"Beëindig kwaliteitsopname"}
									heeftRecht={stopEnabled} rechtNaam={"kwaliteitsopname"}
									popovertekst={!stopEnabled ? "U kunt een kwaliteitsopname pas beëindigen nadat hij gestart is" : undefined}
									onClick={(): void => {
										if (stopEnabled) {
											const reden = state.reden ? state.reden : "Regulier Onderhoud"
											const voorOfNaKalibratie = state.voorOfNaKalibratie ? state.voorOfNaKalibratie : "Voor kalibratie"
											this.setState({
												...state,
												...{
													bezigMetKwaliteitsopname: false,
												},
											})
											this.props.onKwaliteitsopnameAction(BEEINDIG_KWALITEITSOPNAME, reden, voorOfNaKalibratie)
										}
									}}/>
							</Row>
						</Paneel>
					</Col>;
				</Row>
			</div>
		</div>
	}

}