import React, {Component} from "react"
import type {Afspraak} from "../../datatypes/Afspraak"
import {daglijstStatusnaam} from "../../datatypes/Afspraak"
import {datumFormaat} from "../../util/DateUtil"
import type {Onderzoekstatus} from "../../datatypes/Onderzoek"
import {AANVULLENDE_BEELDEN_NODIG_SE} from "../../datatypes/OpschortenReden"
import {AfspraakRijContainerProps} from "./AfspraakRijContainer"

export type AfspraakRijViewStateProps = AfspraakRijContainerProps & {
	onderzoekStatus?: Onderzoekstatus;
	klikbaar: boolean;
};

export type AfspraakRijViewDispatchProps = {
	onRijKlik: (afspraak: Afspraak) => void;
}

export default class AfspraakRijView extends Component<AfspraakRijViewStateProps & AfspraakRijViewDispatchProps> {

	render(): JSX.Element {
		const rijKlikAttributen = !this.props.klikbaar ? {} : {
			onClick: (): void => this.props.onRijKlik(this.props.afspraak),
			style: {
				cursor: "pointer",
			},
		}
		return <tr className={this.className()} {...rijKlikAttributen}>
			<td>{this.props.afspraak.vanafTijd}
				{this.props.client.inTehuis && <i className="fa fa-home px-1 py-1 float-right"/>}
				{!this.props.client.inTehuis && this.props.client.doelgroep === "DUBBELE_TIJD" &&
				<i className="fa fa-clock-o px-1 py-1 float-right"/>}
				{this.props.client.doelgroep === "MINDER_VALIDE" &&
				<i className="fa fa-wheelchair px-1 py-1 float-right"/>}
				{this.props.afspraak.eerderOnderbrokenInZelfdeRonde ?
					<i className="fa fa-step-forward px-1 py-1 float-right">
						<span className="tooltiptext">Aan deze client is in de huidige screeningsronde een onderbroken onderzoek gekoppeld</span></i> : null}
				{this.props.afspraak.eerdereOpschortenReden === AANVULLENDE_BEELDEN_NODIG_SE ?
					<i className="fa fa-plus px-1 py-1 float-right">
						<span className="tooltiptext">
							<p>Bij een beoordeling in de huidige screeningsronde heeft een radioloog aangegeven aanvullende beelden nodig te hebben met opmerking:</p>
							<p>{this.props.afspraak.eerdereOpschortenRedenTekst}</p>
						</span>
					</i> : null}
				{this.props.afspraak.geforceerd ? <i className="fa fa-plus px-1 py-1 float-right">
					<span className="tooltiptext">Geforceerde afspraak, let goed op welke beelden er nog gemaakt moeten worden</span></i> : null}

			</td>
			<td>{this.props.client.voorletters} {this.props.client.aanspreekTussenvoegselEnAchternaam}</td>
			<td>{datumFormaat(this.props.client.geboortedatum)}</td>
			<td>{this.props.client.bsn}</td>
			<td>
				{this.props.afspraak.centralAvailable ? <i className="fa fa-camera-retro px-1 py-1 float-right">
					<span
						className="tooltiptext">Beelden van onderzoek zijn beschikbaar in ScreenIT Centraal</span></i> : null}
				{this.props.afspraak.doorgevoerd ? <i className="fa fa-lock px-1 py-1 float-right">
					<span
						className="tooltiptext">Onderzoek is doorgevoerd en kan niet meer worden aangepast</span></i> : null}
				{daglijstStatusnaam(this.props.afspraak, this.props.onderzoekStatus)}</td>
		</tr>
	}

	className(): string {
		switch (this.props.afspraak.status) {
			case "VERWACHT":
				return "afspraakstatus-verwacht"
			case "INGESCHREVEN":
				return "afspraakstatus-ingeschreven"
			case "ONDERZOEK":
				return "afspraakstatus-onderzoek"
			case "SIGNALEREN":
				return "afspraakstatus-signaleren"
			case "BEEINDIGD":
				if (this.props.onderzoekStatus) {
					switch (this.props.onderzoekStatus) {
						case "ONDERBROKEN":
							return "onderzoeksstatus-onderbroken"
						case "ONVOLLEDIG":
							return "onderzoeksstatus-onvolledig"
						case "AFGEROND":
							return "onderzoeksstatus-afgerond"
						default:
							throw Error("Ongeldige onderzoekstatus bij afgerond onderzoek.")
					}
				}
				return ""
			default:
				return ""
		}
	}

}