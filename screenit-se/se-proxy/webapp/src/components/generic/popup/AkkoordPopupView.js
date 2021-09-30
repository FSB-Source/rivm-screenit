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
import {Button, Modal, ModalBody, ModalFooter, ModalHeader} from 'reactstrap';

type AkkoordPopupProps = {
    titel: string;
    body: React.Component<Object>;
    callback: Function,
    cancelCallback: ?Function,
    visible: boolean,
    akkoordString: string,
    annulerenString: string,

    akkoord: (callback: Function) => void,
    cancel: (cancelCallback: ?Function) => void,
};

export default class AkkoordPopupView extends React.Component<AkkoordPopupProps> {

    constructor(props: AkkoordPopupProps) {
        super(props);
        this.props = props;
    }

    render() {
        return (
            <div>
                <Modal isOpen={this.props.visible} toggle={() => {
                    this.props.cancel(this.props.cancelCallback);
                }} className={''}>
                    <ModalHeader toggle={() => {
                        this.props.cancel(this.props.cancelCallback);
                    }}>{this.props.titel}</ModalHeader>
                    <ModalBody className={'pb-0'}>
                        {this.props.body}
                    </ModalBody>

                    {
                        this.props.akkoordString || this.props.annulerenString ?
                            <ModalFooter>
                                {this.props.akkoordString ?
                                    <Button color="primary" onClick={() => {
                                        this.props.akkoord(this.props.callback);
                                    }}>{this.props.akkoordString}</Button> : null
                                }
                                {this.props.annulerenString ?
                                    <Button color="secondary" onClick={() => {
                                        this.props.cancel(this.props.cancelCallback);
                                    }}>{this.props.annulerenString}</Button> : null
                                }
                            </ModalFooter>
                            : null
                    }
                </Modal>
            </div>
        );
    }
}
