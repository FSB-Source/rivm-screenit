
package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.dao.colon.AfspraakDefinitieDao;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.NieuweIntakeAfspraakMakenReden;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ColoscopieCentrumWrapper;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.planning.AfspraakDefinitie;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.CglibHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ColonClientContactNieuweAfspraakAanmakenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private static final long serialVersionUID = 1L;

	private NieuweIntakeAfspraakMakenReden reden = null;

	private ColonClientNieuweAfspraakMakenPanel afspraakMakenPanel = null;

	@SpringBean
	private OrganisatieZoekService organisatieZoekService;

	@SpringBean
	private AfspraakDefinitieDao afspraakDefinitieDao;

	@SpringBean
	private HibernateService hibernateService;

	public ColonClientContactNieuweAfspraakAanmakenPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);

		ColonDossier colonDossier = client.getObject().getColonDossier();

		IModel<ColonIntakeAfspraak> afspraakModel = Model.of();
		ColonScreeningRonde laatsteScreeningRonde = colonDossier.getLaatsteScreeningRonde();
		if (laatsteScreeningRonde != null)
		{
			ColonIntakeAfspraak laatsteAfspraak = laatsteScreeningRonde.getLaatsteAfspraak();
			if (laatsteAfspraak != null)
			{
				afspraakModel = ModelUtil.sModel(laatsteAfspraak);
			}
			else
			{
				ColonIntakeAfspraak afspraak = new ColonIntakeAfspraak();
				afspraak.setClient(client.getObject());
				afspraak.setColonScreeningRonde(laatsteScreeningRonde);
				ColoscopieCentrumWrapper intakeLocatieWrapper = organisatieZoekService.getNearestIntakeLocatie(client.getObject());
				ColoscopieCentrum intakeLocatie = hibernateService.load(ColoscopieCentrum.class, intakeLocatieWrapper.getId());
				for (Kamer kamer : intakeLocatie.getKamers())
				{
					if (Boolean.TRUE.equals(kamer.getActief()))
					{
						afspraak.setLocation(kamer);
						break;
					}
				}
				List<AfspraakDefinitie> afspraakDefinities = afspraakDefinitieDao.getActieveActieDefinities(intakeLocatie);

				if (afspraakDefinities.size() != 1)
				{
					throw new IllegalStateException("No or too many afspraakDefinities in " + intakeLocatie.getNaam());
				}
				AfspraakDefinitie afspraakDefinitie = afspraakDefinities.get(0);

				afspraak.setStartTime(new Date());
				BigDecimal afstand = intakeLocatieWrapper.getAfstand();
				if (afstand == null)
				{
					afstand = BigDecimal.ZERO;
				}
				afspraak.setAfstand(afstand);
				afspraak.setDefinition(afspraakDefinitie);
				afspraak.addDiscipline(afspraakDefinitie.getDisciplines().get(0));
				afspraak.setBezwaar(false);
				afspraakModel = new CglibHibernateModel<>(afspraak);
			}
		}
		afspraakMakenPanel = new ColonClientNieuweAfspraakMakenPanel("afspraakMaken", afspraakModel);
		add(afspraakMakenPanel);

		add(new ScreenitDropdown<>("reden", new PropertyModel<NieuweIntakeAfspraakMakenReden>(this, "reden"), Arrays.asList(NieuweIntakeAfspraakMakenReden.values()),
			new EnumChoiceRenderer<NieuweIntakeAfspraakMakenReden>()));
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		if (afspraakMakenPanel != null)
		{
			opslaanObjecten.putAll(afspraakMakenPanel.getOpslaanObjecten());
		}
		opslaanObjecten.put(ExtraOpslaanKey.REDEN_NIEUWE_AFSPRAAK, reden);
		return opslaanObjecten;
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		if (afspraakMakenPanel != null)
		{
			return afspraakMakenPanel.getOpslaanMeldingen();
		}
		return new ArrayList<>();
	}

	@Override
	public void validate()
	{
		super.validate();
		if (afspraakMakenPanel != null)
		{
			afspraakMakenPanel.validate();
		}
	}
}
