import type {AlleenOnlineButtonProps} from "./AlleenOnlineButton"
import AlleenOnlineButton from "./AlleenOnlineButton"

export type AutorisatieButtonProps = AlleenOnlineButtonProps & {
	heeftRecht: boolean;
	rechtNaam: string;
};

export default class AutorisatieButton extends AlleenOnlineButton<AutorisatieButtonProps> {

	static defaultProps = {
		online: true,
		className: "float-right",
	}

	render(): JSX.Element {
		const enabled = this.props.heeftRecht && this.props.online
		return this.getButton(enabled)
	}

	getPopoverBody: any = () => {
		return this.props.popovertekst ? this.props.popovertekst : !this.props.heeftRecht ? `Hiervoor heeft u niet de benodigde autorisatie ${this.props.rechtNaam}` : !this.props.online ? this.getPopoverSeOfflineText() : null
	}
}