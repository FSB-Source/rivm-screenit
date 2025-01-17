package nl.rivm.screenit.batch.jobs.cervix.selectie.laatsterondesluitenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.algemeen.DossierSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.specification.algemeen.ScreeningRondeSpecification;
import nl.rivm.screenit.specification.cervix.CervixDossierSpecification;
import nl.rivm.screenit.specification.cervix.CervixScreeningRondeSpecification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Component
public class CervixLaatsteRondeSluitenReader extends BaseSpecificationScrollableResultReader<CervixScreeningRonde>
{

	private final ICurrentDateSupplier dateSupplier;

	public CervixLaatsteRondeSluitenReader(ICurrentDateSupplier dateSupplier)
	{
		super.setFetchSize(50);
		this.dateSupplier = dateSupplier;
	}

	@Override
	protected Specification<CervixScreeningRonde> createSpecification()
	{
		var vandaag = dateSupplier.getLocalDate();
		var geboortedatumMaximaal = vandaag.minusYears(CervixLeeftijdcategorie._65.getLeeftijd());
		var geboortedatumMaximaalVervolgonderzoekNegatief = vandaag.minusYears(CervixLeeftijdcategorie._70.getLeeftijd());

		return (r, q, cb) ->
		{
			var dossierJoin = join(r, CervixScreeningRonde_.dossier);
			var clientJoin = join(dossierJoin, CervixDossier_.client);
			var persoonJoin = join(clientJoin, Client_.persoon);

			return cb.and(
				cb.or(
					CervixDossierSpecification.magNietDeelnemen(vandaag).with(root -> dossierJoin).toPredicate(r, q, cb),
					PersoonSpecification.isGeborenVoorOfOp(geboortedatumMaximaalVervolgonderzoekNegatief).with(root -> persoonJoin).toPredicate(r, q, cb),
					cb.and(
						CervixScreeningRondeSpecification.heeftNietLeeftijdCategorie(CervixLeeftijdcategorie._65).toPredicate(r, q, cb),
						PersoonSpecification.isGeborenVoorOfOp(geboortedatumMaximaal).with(root -> persoonJoin).toPredicate(r, q, cb)
					)
				),
				DossierSpecification.heeftStatus(DossierStatus.ACTIEF).with(root -> dossierJoin).toPredicate(r, q, cb),
				ScreeningRondeSpecification.isLopend().toPredicate(r, q, cb)
			);
		};
	}
}
