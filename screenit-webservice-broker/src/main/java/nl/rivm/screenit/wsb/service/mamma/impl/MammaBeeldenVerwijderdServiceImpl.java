package nl.rivm.screenit.wsb.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.wsb.service.mamma.MammaBeeldenVerwijderdService;
import nl.rivm.screenit.wsb.service.mamma.MammaHL7v24Service;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class MammaBeeldenVerwijderdServiceImpl extends MammaHL7V24ServiceImpl implements MammaBeeldenVerwijderdService
{
	@Override
	List<MammaHL7v24ORMBerichtStatus> getAcceptedOrmBerichtStatussen()
	{
		return Arrays.asList(MammaHL7v24ORMBerichtStatus.DELETED, MammaHL7v24ORMBerichtStatus.ERROR);
	}
}
