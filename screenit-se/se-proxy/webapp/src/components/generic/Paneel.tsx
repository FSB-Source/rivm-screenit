import * as React from "react"
import {Container} from "reactstrap"

export type PaneelProps = {
	children?: React.ReactNode;
	className?: string;
};

export default class Paneel extends React.Component<PaneelProps> {
	render(): JSX.Element {
		return <div className={["paneel", this.props.className].join(" ")}>
			<Container fluid={true}>
				{this.props.children}
			</Container>
		</div>
	}

}