import React, {MouseEvent} from "react"
import {Button, Popover, PopoverBody, PopoverHeader} from "reactstrap"
import {Placement} from "popper.js"
import classNames from "classnames"

export type AlleenOnlineButtonProps = {
	id?: string;
	label: string;
	online: boolean;
	placementFeedback?: Placement;
	color?: string;
	center?: boolean;
	className?: string;
	popovertekst?: string;
	onClick: (event: MouseEvent<HTMLButtonElement>) => void;
};

export type AlleenOnlineButtonState = {
	popoverOpen: boolean;
};

export default class AlleenOnlineButton<P extends AlleenOnlineButtonProps> extends React.Component<P, AlleenOnlineButtonState> {

	public readonly state: AlleenOnlineButtonState = {
		popoverOpen: false,
	}

	constructor(props: P) {
		super(props)
		this.toggle = this.toggle.bind(this)
	}

	toggle = (): void => {
		this.setState({
			popoverOpen: !this.state.popoverOpen,
		})
	}

	render(): JSX.Element {
		const enabled = this.props.online
		return this.getButton(enabled)
	}

	getButton = (enabled: boolean): JSX.Element => {
		return <div className={classNames("alleen-online-btn", this.props.center ? "mx-auto" : undefined)}>
			<span>
				<Button
					id={this.props.id ? this.props.id : "Popover-btn"}
					className={`${enabled ? "" : " disabled "} ${this.props.className || ""}`}
					color={this.props.color ? this.props.color : "primary-se"}
					onClick={enabled ? this.props.onClick : this.toggle}>{this.props.label}
				</Button>

				<Popover
					id={this.props.id ? `${this.props.id}Popover` : "Popover"}
					placement={this.props.placementFeedback || "right-start"}
					isOpen={this.state.popoverOpen && !!this.getPopoverBody()}
					target={this.props.id ? this.props.id : "Popover-btn"} trigger="click">
					<PopoverHeader>Actie niet beschikbaar</PopoverHeader>
					<PopoverBody>{this.getPopoverBody()}</PopoverBody>
				</Popover>
			</span>
		</div>
	}

	getPopoverBody = (): string | null => {
		return this.props.popovertekst ? this.props.popovertekst : !this.props.online ? this.getPopoverSeOfflineText() : null
	}

	getPopoverSeOfflineText = (): string => {
		return "Wanneer de SE offline is kan deze actie niet worden uitgevoerd"
	}
}