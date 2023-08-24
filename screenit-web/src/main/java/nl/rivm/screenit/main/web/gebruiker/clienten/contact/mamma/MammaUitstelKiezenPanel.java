
package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.dto.mamma.afspraken.MammaStandplaatsPeriodeMetAfstandDto;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaUitstelKiezenPanel extends AbstractClientContactActiePanel<Client>
{

	private static final long serialVersionUID = 1L;

	private Panel nieuweUitstelPanel;

	private MammaUitstelZoekenPanel uitstelZoekenPanel;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaBaseFactory baseFactory;

	@SpringBean
	private MammaBaseUitstelService baseUitstelService;

	public MammaUitstelKiezenPanel(String id, IModel<Client> clientModel)
	{
		super(id, clientModel);

		nieuweUitstelPanel = new EmptyPanel("nieuweUitstelPanel");
		nieuweUitstelPanel.setOutputMarkupId(true);
		add(nieuweUitstelPanel);

		uitstelZoekenPanel = new MammaUitstelZoekenPanel("uitstelZoekenPanel", clientModel)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void nieuwUitstel(AjaxRequestTarget target, IModel<MammaStandplaatsPeriodeMetAfstandDto> model, Date zoekDatum)
			{
				Client client = MammaUitstelKiezenPanel.this.getModelObject();
				MammaScreeningRonde screeningRonde = client.getMammaDossier().getLaatsteScreeningRonde();
				boolean isNieuweUitstel = true;
				IModel<MammaUitstel> uitstelModel;
				MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, model.getObject().getStandplaatsPeriodeId());
				MammaStandplaats standplaats = standplaatsPeriode.getStandplaatsRonde()
					.getStandplaats();

				uitstelModel = ModelUtil.cModel(baseUitstelService.getOfMaakMammaUitstel(screeningRonde, standplaats, zoekDatum));

				if (screeningRonde.getLaatsteUitstel() != null && uitstelModel.getObject().equals(screeningRonde.getLaatsteUitstel()))
				{
					isNieuweUitstel = false;
				}

				MammaUitstelPanel uitstelPanel = new MammaUitstelPanel("nieuweUitstelPanel", uitstelModel, isNieuweUitstel, standplaatsPeriode)
				{
					@Override
					protected void wijzigMoment(AjaxRequestTarget target)
					{
						Panel emptyPanel = new EmptyPanel("nieuweUitstelPanel");
						emptyPanel.setOutputMarkupId(true);
						nieuweUitstelPanel.replaceWith(emptyPanel);
						nieuweUitstelPanel = emptyPanel;
						uitstelZoekenPanel.setVisible(true);
						target.add(nieuweUitstelPanel);
						target.add(uitstelZoekenPanel);
					}
				};
				this.setVisible(false);
				nieuweUitstelPanel.replaceWith(uitstelPanel);
				nieuweUitstelPanel = uitstelPanel;
				target.add(nieuweUitstelPanel);
				target.add(this);
			}

		};
		uitstelZoekenPanel.setOutputMarkupPlaceholderTag(true);

		add(uitstelZoekenPanel);
	}

	@Override
	public void validate()
	{
		if (nieuweUitstelPanel instanceof EmptyPanel)
		{
			error("U heeft geen nieuw uitstel gekozen.");
		}
		else if (nieuweUitstelPanel instanceof AbstractClientContactActiePanel)
		{
			((AbstractClientContactActiePanel) nieuweUitstelPanel).validate();
		}
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> opslaanMeldingen = super.getOpslaanMeldingen();
		if (nieuweUitstelPanel instanceof AbstractClientContactActiePanel)
		{
			opslaanMeldingen.addAll(((AbstractClientContactActiePanel) nieuweUitstelPanel).getOpslaanMeldingen());
		}
		return opslaanMeldingen;
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = new HashMap<>();

		if (nieuweUitstelPanel instanceof AbstractClientContactActiePanel)
		{
			opslaanObjecten.putAll(((AbstractClientContactActiePanel) nieuweUitstelPanel).getOpslaanObjecten());
		}
		return opslaanObjecten;
	}

}
