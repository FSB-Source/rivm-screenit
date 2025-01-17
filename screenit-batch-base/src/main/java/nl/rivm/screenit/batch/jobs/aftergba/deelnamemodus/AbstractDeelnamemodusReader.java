package nl.rivm.screenit.batch.jobs.aftergba.deelnamemodus;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.util.List;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.DeelnamemodusDossier;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.joinByString;
import static nl.rivm.screenit.specification.algemeen.DossierSpecification.heeftDeelnamemodus;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.heeftGeslachtIn;

public abstract class AbstractDeelnamemodusReader<D extends Dossier<?, ?> & DeelnamemodusDossier> extends BaseSpecificationScrollableResultReader<D>
{
	@Override
	protected Specification<D> createSpecification()
	{
		var manOfOnbekendMetDeelnamemodusStandaard = heeftDeelnamemodusEnGeslacht(List.of(Geslacht.MAN, Geslacht.ONBEKEND), Deelnamemodus.STANDAARD);
		var vrouwMetSelectieblokkade = heeftDeelnamemodusEnGeslacht(List.of(Geslacht.VROUW), Deelnamemodus.SELECTIEBLOKKADE);
		return manOfOnbekendMetDeelnamemodusStandaard.or(vrouwMetSelectieblokkade);
	}

	private ExtendedSpecification<D> heeftDeelnamemodusEnGeslacht(List<Geslacht> geslachten, Deelnamemodus deelnamemodus)
	{
		return heeftGeslachtIn(geslachten).<D> with(this::persoonJoin)
			.and(heeftDeelnamemodus(deelnamemodus));
	}

	private Join<Client, GbaPersoon> persoonJoin(From<?, ? extends D> dossierRoot)
	{
		Join<?, Client> clientJoin = joinByString(dossierRoot, MammaDossier_.CLIENT, JoinType.INNER);
		return join(clientJoin, Client_.persoon);
	}
}
