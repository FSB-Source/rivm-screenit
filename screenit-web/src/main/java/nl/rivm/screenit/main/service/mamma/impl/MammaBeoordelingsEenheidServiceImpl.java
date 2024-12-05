package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Collections;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.dao.InstellingDao;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingsEenheidService;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.repository.algemeen.BeoordelingsEenheidRepository;
import nl.rivm.screenit.repository.mamma.MammaBaseOnderzoekRepository;
import nl.rivm.screenit.repository.mamma.MammaScreeningsEenheidRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.BeoordelingsEenheidSpecification;
import nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification;
import nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.algemeen.BeoordelingsEenheidSpecification.heeftCentraleEenheidIn;
import static nl.rivm.screenit.specification.algemeen.BeoordelingsEenheidSpecification.heeftScreeningOrganisatie;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.heeftGeenBeoordelingStatusIn;
import static nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification.isDoorgevoerd;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.heeftBeoordelingsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.heeftTijdelijkeBeoordelingsEenheidActiefOpMoment;

@Service
@RequiredArgsConstructor
public class MammaBeoordelingsEenheidServiceImpl implements MammaBeoordelingsEenheidService
{
	private final InstellingDao instellingDao;

	private final MammaScreeningsEenheidRepository screeningsEenheidRepository;

	private final BeoordelingsEenheidRepository beoordelingsEenheidRepository;

	private final ICurrentDateSupplier currentDateSuplier;

	private final MammaBaseOnderzoekRepository onderzoekRepository;

	@Override
	public long getAantalActieveGekoppeldeScreeningsEenheden(BeoordelingsEenheid beoordelingsEenheid)
	{
		var nu = currentDateSuplier.getLocalDate();
		return screeningsEenheidRepository.count(MammaScreeningsEenheidSpecification.isActief()
			.and(heeftBeoordelingsEenheid(beoordelingsEenheid).or(heeftTijdelijkeBeoordelingsEenheidActiefOpMoment(beoordelingsEenheid, nu)))
		);
	}

	@Override
	public long getAantalNietAfgerondeGekoppeldeOnderzoeken(BeoordelingsEenheid beoordelingsEenheid)
	{
		return onderzoekRepository.count(isDoorgevoerd(false).and(MammaOnderzoekSpecification.heeftBeoordelingsEenheid(beoordelingsEenheid)));
	}

	@Override
	public long getAantalNietAfgerondeGekoppeldeBeoordelingen(BeoordelingsEenheid beoordelingsEenheid)
	{
		return onderzoekRepository.count(
			MammaOnderzoekSpecification.heeftBeoordelingsEenheid(beoordelingsEenheid).and(heeftGeenBeoordelingStatusIn(MammaBeoordelingStatus.eindStatussen())));

	}

	@Override
	public String magWordenGeactiveerd(BeoordelingsEenheid beoordelingsEenheid)
	{
		if (beoordelingsEenheid.getParent() != null && !beoordelingsEenheid.getParent().getActief())
		{
			return "beoordelingsEenheid.activeren.inactiveCE";
		}
		return "";
	}

	@Override
	public String magWordenGeinactiveerd(BeoordelingsEenheid beoordelingsEenheid)
	{
		if (getAantalActieveGekoppeldeScreeningsEenheden(beoordelingsEenheid) > 0)
		{
			return "beoordelingsEenheid.inactiveren.actieveSE";
		}
		if (getAantalNietAfgerondeGekoppeldeOnderzoeken(beoordelingsEenheid) > 0)
		{
			return "beoordelingsEenheid.inactiveren.actieveOnderzoeken";
		}
		if (getAantalNietAfgerondeGekoppeldeBeoordelingen(beoordelingsEenheid) > 0)
		{
			return "beoordelingsEenheid.inactiveren.actieveBeoordelingen";
		}
		return "";
	}

	@Override
	public List<BeoordelingsEenheid> getBeoordelingsEenheden(Instelling instelling)
	{
		if (instelling == null)
		{
			return Collections.emptyList();
		}
		instelling = (Instelling) HibernateHelper.deproxy(instelling);
		OrganisatieType organisatieType = instelling.getOrganisatieType();
		switch (organisatieType)
		{
		case RIVM:
		case KWALITEITSPLATFORM:
			return instellingDao.getActieveInstellingen(BeoordelingsEenheid.class);
		case BEOORDELINGSEENHEID:
			return Collections.singletonList((BeoordelingsEenheid) instelling);
		case SCREENINGSORGANISATIE:
			return getActieveBeoordelingsEenhedenVoorScreeningsOrganisatie(instelling);
		default:
			return Collections.emptyList();
		}
	}

	@Override
	public List<BeoordelingsEenheid> getBeoordelingsEenheden(Instelling instelling, List<CentraleEenheid> centraleEenheden)
	{
		if (instelling == null)
		{
			return Collections.emptyList();
		}
		OrganisatieType organisatieType = instelling.getOrganisatieType();
		instelling = (Instelling) HibernateHelper.deproxy(instelling);
		switch (organisatieType)
		{
		case RIVM:
		case KWALITEITSPLATFORM:
		case SCREENINGSORGANISATIE:
			if (centraleEenheden.isEmpty())
			{
				return Collections.emptyList();
			}
			return getActieveBeoordelingsEenhedenVoorCentraleEenheden(centraleEenheden);
		case BEOORDELINGSEENHEID:
			return Collections.singletonList((BeoordelingsEenheid) instelling);
		default:
			return Collections.emptyList();
		}
	}

	private List<BeoordelingsEenheid> getActieveBeoordelingsEenhedenVoorScreeningsOrganisatie(Instelling instelling)
	{
		return beoordelingsEenheidRepository.findAll(
			BeoordelingsEenheidSpecification.isActief(true).and(heeftScreeningOrganisatie(instelling)),
			Sort.by(Instelling_.NAAM));
	}

	private List<BeoordelingsEenheid> getActieveBeoordelingsEenhedenVoorCentraleEenheden(List<CentraleEenheid> centraleEenheden)
	{
		return beoordelingsEenheidRepository.findAll(
			BeoordelingsEenheidSpecification.isActief(true).and(heeftCentraleEenheidIn(centraleEenheden)),
			Sort.by(Instelling_.NAAM));
	}
}
