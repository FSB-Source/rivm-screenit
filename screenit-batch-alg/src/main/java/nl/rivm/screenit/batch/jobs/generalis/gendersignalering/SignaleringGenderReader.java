package nl.rivm.screenit.batch.jobs.generalis.gendersignalering;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static javax.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftActieveClient;
import static nl.rivm.screenit.specification.algemeen.DossierSpecification.heeftDeelnamemodus;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.isGeborenNa;

@Component
@AllArgsConstructor
public class SignaleringGenderReader extends BaseSpecificationScrollableResultReader<Client>
{
	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<Client> createSpecification()
	{
		var heeftMammaDeelnameModus = heeftDeelnamemodus(Deelnamemodus.SELECTIEBLOKKADE).with(Client_.mammaDossier, LEFT);
		var heeftCervixDeelnameModus = heeftDeelnamemodus(Deelnamemodus.SELECTIEBLOKKADE).with(Client_.cervixDossier, LEFT);

		return heeftGeenBriefSignaleringGender()
			.and(heeftMammaDeelnameModus
				.or(heeftCervixDeelnameModus))
			.and(isGeborenNa(maxGeboorteDatumVoorDoelgroep()).with(Client_.persoon))
			.and(heeftActieveClient());
	}

	private LocalDate maxGeboorteDatumVoorDoelgroep()
	{
		var mammaTotEnMetLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());
		var cervixTotEnMetLeeftijd = CervixLeeftijdcategorie._70.getLeeftijd();
		var signalerenTotLeeftijd = Math.max(mammaTotEnMetLeeftijd, cervixTotEnMetLeeftijd) + 1;
		return currentDateSupplier.getLocalDate().minusYears(signalerenTotLeeftijd);
	}

	private Specification<Client> heeftGeenBriefSignaleringGender()
	{
		return (r, q, cb) ->
		{
			var algemeneBriefJoin = join(r, Client_.algemeneBrieven, LEFT);
			algemeneBriefJoin.on(cb.equal(algemeneBriefJoin.get(Brief_.briefType), BriefType.CLIENT_SIGNALERING_GENDER));
			return cb.isNull(algemeneBriefJoin.get(TablePerClassHibernateObject_.id));
		};
	}
}
