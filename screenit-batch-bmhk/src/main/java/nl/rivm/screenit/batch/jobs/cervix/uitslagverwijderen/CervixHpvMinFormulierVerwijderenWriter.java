package nl.rivm.screenit.batch.jobs.cervix.uitslagverwijderen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;

public class CervixHpvMinFormulierVerwijderenWriter extends BaseWriter<CervixLabformulier>
{

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private HibernateService hibernateService;

	@Override
	protected void write(CervixLabformulier labformulier) throws Exception
	{

		labformulier.setKlachtenGeen(null);
		labformulier.setKlachtenContactbloedingen(null);
		labformulier.setKlachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak(null);
		labformulier.setKlachtenIntermenstrueelBloedverlies(null);
		labformulier.setKlachtenPostmenopauzaalBloedverlies(null);
		labformulier.setKlachtenAndersNamelijk(null);
		labformulier.setMenstruatieNormaal(null);
		labformulier.setMenstruatieGeenMenstruatie(null);
		labformulier.setMenstruatieMenopauze(null);
		labformulier.setMenstruatiePostmenopauze(null);
		labformulier.setDatumLaatsteMenstruatie(null);
		labformulier.setAnticonceptieGeen(null);
		labformulier.setAnticonceptiePil(null);
		labformulier.setAnticonceptieIudKoper(null);
		labformulier.setAnticonceptieIudMirena(null);
		labformulier.setAnticonceptieAnders(null);

		labformulier.setGebruikHormonenJaVanwegeOvergangsklachten(null);
		labformulier.setGebruikHormonenJaVanwegeBorstkanker(null);
		labformulier.setGebruikHormonenJaVanwege(null);
		labformulier.setGebruikHormonenGeen(null);

		labformulier.setAspectCervixNietGezien(null);
		labformulier.setAspectCervixAbnormaalOfVerdachtePortio(null);
		labformulier.setAspectCervixNormaal(null);
		labformulier.setOpmerkingen(null);

		labformulier.setDatumGewist(dateSupplier.getDate());

		hibernateService.saveOrUpdate(labformulier);
		aantalContextOphogen(CervixHpvMinFormulierVerwijderenConstants.AANTAL_FORMULIEREN_GEWIST);
	}
}
