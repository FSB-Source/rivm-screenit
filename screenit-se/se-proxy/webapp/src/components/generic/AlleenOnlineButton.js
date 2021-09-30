/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import React from 'react';
import {Button, Popover, PopoverBody, PopoverHeader} from 'reactstrap';

export type AlleenOnlineButtonProps = {
    id?: string,
    label: string;
    online: boolean;
    placementFeedback?: string,
    color?: string,
    center?: boolean,
    className?: string,
    popovertekst?: ?string,
    onClick: (event: Event) => mixed;
}

export type AlleenOnlineButtonState = {
    popoverOpen: boolean
}

export default class AlleenOnlineButton<Props, State> extends React.Component<any, any> {

    constructor(props: AlleenOnlineButtonProps) {
        super(props);

        this.toggle = this.toggle.bind(this);
        this.state = {
            popoverOpen: false,
        };
    }

    toggle = () => {
        this.setState({
            popoverOpen: !this.state.popoverOpen,
        });
    };

    render() {
        const enabled = this.props.online;
        return this.getButton(enabled);
    }

    getButton = (enabled: boolean) => {
        return <div className={this.props.center ? 'mx-auto' : null}>
                <span>
                    <Button id={this.props.id ? this.props.id : 'Popover-btn'}
                            className={(enabled ? '' : ' disabled ') + 'float-right ' + (this.props.className || '')}
                            color={this.props.color ? this.props.color : 'primary-se'}
                            onClick={
                                enabled ? this.props.onClick.bind(this)
                                    : this.toggle
                            }>{this.props.label}
                    </Button>

                    <Popover id={this.props.id ? this.props.id + 'Popover' : 'Popover'}
                             placement={this.props.placementFeedback || 'right-start'}
                             isOpen={this.state.popoverOpen && this.getPopoverBody()}
                             target={this.props.id ? this.props.id : 'Popover-btn'}
                             trigger="click">
                        <PopoverHeader>Actie niet beschikbaar</PopoverHeader>
                        <PopoverBody>{this.getPopoverBody()}</PopoverBody>
                    </Popover>
                </span>
        </div>;
    };

    getPopoverBody: any = () => {
        return this.props.popovertekst ? this.props.popovertekst : !this.props.online ? this.getPopoverSeOfflineText() : null;
    };

    getPopoverSeOfflineText = () => {
        return 'Wanneer de SE offline is kan deze actie niet worden uitgevoerd'
    }
}
