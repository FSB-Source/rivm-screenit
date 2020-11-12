package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.service.UitnodigingenCleanUpService;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.model.colon.ColonMergedBrieven;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.hibernate.Criteria;
import org.hibernate.StatelessSession;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class UitnodigingenCleanUpServiceImpl implements UitnodigingenCleanUpService
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	public <MB extends MergedBrieven<?>> Criteria cleanUpUitnodigingen(StatelessSession session, Class<MB> mergedBrievenClass, int minimaleBestaanOpFilestore)
	{
		Criteria crit = session.createCriteria(mergedBrievenClass);

		crit.add(Restrictions.eq("verwijderd", Boolean.FALSE));

		crit.add(Restrictions.le("creatieDatum", currentDateSupplier.getDateTime().minusDays(minimaleBestaanOpFilestore).toDate()));

		Bevolkingsonderzoek bevolkingsonderzoek = bepaalBevolkingsonderzoek(mergedBrievenClass);
		crit.add(Restrictions.in("briefType", BriefType.getBriefTypesMetOrganisatieType(true, OrganisatieType.INPAKCENTRUM, bevolkingsonderzoek)));

		return crit;
	}

	private <MB extends MergedBrieven<?>> Bevolkingsonderzoek bepaalBevolkingsonderzoek(Class<MB> mergedBrievenClass)
	{
		if (ColonMergedBrieven.class.equals(mergedBrievenClass))
		{
			return Bevolkingsonderzoek.COLON;
		}
		else if (CervixMergedBrieven.class.equals(mergedBrievenClass))
		{
			return Bevolkingsonderzoek.CERVIX;
		}
		else
		{
			throw new IllegalStateException("Merged brieven class is ongeldig: " + mergedBrievenClass);
		}
	}
}
