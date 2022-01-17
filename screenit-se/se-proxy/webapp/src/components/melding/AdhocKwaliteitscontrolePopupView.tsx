import {Button, Col, ModalFooter, Row} from "reactstrap"
import {dispatchActions} from "../../util/DispatchUtil"
import {createActionClearPopup} from "../../actions/PopupActions"
import {store} from "../../Store"
import {showErrorToast} from "../../util/ToastUtil"
import {verstuurAdHocVerzoek} from "../../restclient/AdhocMeekijkverzoekRestClient"
import React from "react"

type AdhocKwaliteitscontrolePopupProps = {
	afspraakId: number;
};

type AdhocKwaliteitscontrolePopupState = {
	reden?: string;
};

const adhocRedenen = ["Regulier Onderhoud", "Verplaatsing", "Storing", "Reparatie", "Vervanging röntgenbuis", "Vervanging detector (LRCB)", "Foutieve opname in set", "Zichtbare Beeldverstoring", "Error tijdens kalibratie", "Graag SE bellen ", "Op verzoek LRCB"]

export default class AdhocKwaliteitscontrolePopupView extends React.Component<AdhocKwaliteitscontrolePopupProps, AdhocKwaliteitscontrolePopupState> {

	constructor(props: AdhocKwaliteitscontrolePopupProps) {
		super(props)
		this.state = {
			reden: undefined,
		}
	}

	render(): JSX.Element {
		return <div>
			<p>Selecteer een reden en verstuur het verzoek naar het LRCB.</p>
			<Row className={"popupRow"}>
				{this.maakRedenKeuzeRijen()}
			</Row>
			{this.state.reden ? <p className={"geselecteerde-adhoc-reden-header"}>Gekozen reden:</p> : null}
			<p className={"geselecteerde-adhoc-reden"}>{this.state.reden}</p>
			<ModalFooter>
				<Button
					color={this.state.reden ? "primary" : "secondary"}
					onClick={(): void => {
						if (this.state.reden) {
							this.verstuurVerzoek(this.state.reden, this.props.afspraakId)
							dispatchActions(store.dispatch, createActionClearPopup())
						} else {
							showErrorToast("Selecteer een reden")
						}
					}}>Verzoek versturen</Button>
				<Button
					color="secondary"
					onClick={(): void => {
						dispatchActions(store.dispatch, createActionClearPopup())
					}}>Annuleren</Button>

			</ModalFooter>
		</div>
	}

	maakRedenKeuzeRijen(): React.ReactNode[] {
		const result: React.ReactNode[] = []
		adhocRedenen.forEach(reden => {
			result.push(<Col key={reden} md={12} className="adhocKeuzeRow" onClick={(): void => {
				this.selecteerReden(reden)
			}}>
				{reden}
			</Col>)
		})
		return result
	}

	selecteerReden(reden: string): void {
		this.setState({
			reden: reden,
		})
	}

	verstuurVerzoek(reden: string, afspraakId: number): void {
		verstuurAdHocVerzoek(reden, afspraakId)
	}

}