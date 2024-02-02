package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.service.BaseDossierService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.REQUIRED)
public class BaseDossierServiceImpl implements BaseDossierService
{
	private final UploadDocumentService uploadDocumentService;

	private final HibernateService hibernateService;

	private final ClientService clientService;

	@Override
	public <D extends Dossier<?, A>, A extends Afmelding<?, ?, ?>> void verwijderNietLaatsteDefinitieveAfmeldingenUitDossier(D dossier)
	{
		colonSpecifiekeVerwijderVerwerking(dossier);
		for (A afmelding : dossier.getAfmeldingen())
		{
			if (!afmelding.equals(dossier.getLaatsteAfmelding()) && afmelding.getHeraanmeldDatum() != null)
			{
				verwijderAfmeldingEnDocumenten(afmelding);
			}
		}
	}

	private <D extends Dossier<?, A>, A extends Afmelding<?, ?, ?>> void colonSpecifiekeVerwijderVerwerking(D dossier)
	{
		if (dossier instanceof ColonDossier)
		{
			ColonDossier colonDossier = (ColonDossier) dossier;

			for (ColonAfmelding afmelding : colonDossier.getAfmeldingen())
			{
				if (ColonAfmeldingReden.PROEF_BEVOLKINGSONDERZOEK.equals(afmelding.getReden()))
				{
					if (clientService.isHandtekeningBriefGebruiktBijMeedereColonAfmeldingen(afmelding.getHandtekeningDocumentAfmelding(), "handtekeningDocumentAfmelding"))
					{
						afmelding.setHandtekeningDocumentAfmelding(null);
						hibernateService.saveOrUpdate(afmelding);
					}
					if (clientService.isHandtekeningBriefGebruiktBijMeedereColonAfmeldingen(afmelding.getHandtekeningDocumentHeraanmelding(),
						"handtekeningDocumentHeraanmelding"))
					{
						afmelding.setHandtekeningDocumentHeraanmelding(null);
						hibernateService.saveOrUpdate(afmelding);
					}
				}
			}
		}
	}

	@Override
	public <SR extends ScreeningRonde<?, ?, A, ?>, A extends Afmelding<SR, ?, ?>> void verwijderAlleAfmeldingenUitRonde(SR ronde)
	{
		ronde.setLaatsteAfmelding(null);
		for (A afmelding : ronde.getAfmeldingen())
		{
			verwijderAfmeldingEnDocumenten(afmelding);
		}
	}

	@Override
	public <D extends Dossier<?, A>, A extends Afmelding<?, ?, ?>> void verwijderLaatsteAfmelding(D dossier)
	{
		if (dossier.getLaatsteAfmelding() != null)
		{
			verwijderAfmeldingEnDocumenten(dossier.getLaatsteAfmelding());
		}
	}

	private <A extends Afmelding<?, ?, ?>> void verwijderAfmeldingEnDocumenten(A afmelding)
	{
		afmelding.setAfmeldingAanvraag(null);
		afmelding.setAfmeldingBevestiging(null);
		var documentAfmelding = afmelding.getHandtekeningDocumentAfmelding();
		if (documentAfmelding != null)
		{
			uploadDocumentService.delete(documentAfmelding);
		}
		afmelding.setHeraanmeldAanvraag(null);
		afmelding.setHeraanmeldBevestiging(null);
		var documentHeraanmelding = afmelding.getHandtekeningDocumentHeraanmelding();
		if (documentHeraanmelding != null)
		{
			uploadDocumentService.delete(documentHeraanmelding);
		}
		hibernateService.deleteAll(afmelding.getBrieven());
		hibernateService.delete(afmelding);
	}
}
