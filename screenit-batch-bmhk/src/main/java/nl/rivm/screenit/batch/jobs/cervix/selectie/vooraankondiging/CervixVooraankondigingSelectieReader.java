package nl.rivm.screenit.batch.jobs.cervix.selectie.vooraankondiging;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.cervix.CervixLabPartitioner;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.DossierSpecification;
import nl.rivm.screenit.specification.algemeen.GemeenteSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.specification.cervix.CervixDossierSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.model.DossierStatus.ACTIEF;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftActieveClient;

@Component
public class CervixVooraankondigingSelectieReader extends BaseSpecificationScrollableResultReader<Client>
{
	private final ICurrentDateSupplier dateSupplier;

	private final SimplePreferenceService preferenceService;

	public CervixVooraankondigingSelectieReader(ICurrentDateSupplier dateSupplier, SimplePreferenceService preferenceService)
	{
		super.setFetchSize(50);
		this.dateSupplier = dateSupplier;
		this.preferenceService = preferenceService;
	}

	@Override
	protected Specification<Client> createSpecification()
	{
		var stepContext = getStepExecutionContext();
		var bmhkLabId = (Long) stepContext.get(CervixLabPartitioner.KEY_BMHK_LAB);
		var vandaag = dateSupplier.getLocalDate();

		var dagenVoorDeVooraankondiging = preferenceService.getInteger(PreferenceKey.CERVIX_VOORAANKONDIGINGS_PERIODE.name());
		var exactDertigJaarGeleden = vandaag.minusYears(CervixLeeftijdcategorie._30.getLeeftijd());
		var dertigJaarGeledenPlusVooraankondigingsDagen =
			dagenVoorDeVooraankondiging != null ? exactDertigJaarGeleden.plusDays(dagenVoorDeVooraankondiging) : exactDertigJaarGeleden;

		return heeftActieveClient()
			.and(CervixDossierSpecification.heeftGeenVooraankondigingsBrief().with(Client_.cervixDossier))
			.and(DossierSpecification.heeftStatus(ACTIEF).with(Client_.cervixDossier))
			.and(CervixDossierSpecification.wachtOpStartProject(false).with(Client_.cervixDossier))
			.and(CervixDossierSpecification.heeftNietDeelnamemodus(Deelnamemodus.SELECTIEBLOKKADE).with(Client_.cervixDossier))
			.and(PersoonSpecification.isGeborenNa(exactDertigJaarGeleden).with(Client_.persoon))
			.and(PersoonSpecification.isGeborenVoorOfOp(dertigJaarGeledenPlusVooraankondigingsDagen).with(Client_.persoon))
			.and(GemeenteSpecification.heeftScreeningOrganisatie().with(r -> gemeenteJoin(r)))
			.and(GemeenteSpecification.heeftBmhkLaboratorium(bmhkLabId).with(r -> gemeenteJoin(r)));
	}

	private static Join<?, Gemeente> gemeenteJoin(From<?, ? extends Client> r)
	{
		return join(join(persoonJoin(r), GbaPersoon_.gbaAdres), BagAdres_.gbaGemeente);
	}

	private static Join<? extends Client, GbaPersoon> persoonJoin(From<?, ? extends Client> r)
	{
		return join(r, Client_.persoon);
	}
}
