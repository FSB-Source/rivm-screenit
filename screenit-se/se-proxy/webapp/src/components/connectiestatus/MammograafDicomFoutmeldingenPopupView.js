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
import type {MammograafDicomMessageError, MammograafDicomMessageType} from '../../datatypes/connectiestatus/MammograafDicomMessageError';
import moment from 'moment';

export type MammograafDicomFoutmeldingenPopupViewProps = {
    messageType: MammograafDicomMessageType;
    errors: Array<MammograafDicomMessageError>;
}

export default class MammograafDicomFoutmeldingenPopupView extends React.Component<MammograafDicomFoutmeldingenPopupViewProps> {

    constructor(props: MammograafDicomFoutmeldingenPopupViewProps) {
        super(props);
        this.props = props;
    }

    render() {
        return <div>
            {this.props.messageType === 'MPPS' ? <div>
                {this.props.errors.reverse().map((e: MammograafDicomMessageError) => {
                    return <p key={e.timestamp} className={'mammograafDicomMessageErrorText'}><b>{moment(e.timestamp).fromNow()}</b>: {e.message}</p>;
                })}
            </div> : <div>
                {this.props.errors.reverse().map((e: MammograafDicomMessageError) => {
                    return <p key={e.timestamp} className={'mammograafDicomMessageErrorText'}><b>{moment(e.timestamp).fromNow()}</b>: DMWL mislukt door mismatch in keys. {e.message}</p>;
                })}
            </div>}
        </div>;
    }

}
