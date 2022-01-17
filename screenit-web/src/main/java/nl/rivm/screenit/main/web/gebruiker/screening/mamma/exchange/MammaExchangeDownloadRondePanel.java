package nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.service.RondeNummerService;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.topicuszorg.wicket.search.column.HibernateCheckBoxListContainer;

import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaExchangeDownloadRondePanel extends GenericPanel<MammaScreeningRonde>
{

	@SpringBean
	private RondeNummerService rondeNummerService;

	public MammaExchangeDownloadRondePanel(String id, HibernateCheckBoxListContainer<MammaOnderzoek> selectedOnderzoeken, IModel<MammaScreeningRonde> model)
	{
		super(id, model);
		add(new Label("rondeNr", Model.of(rondeNummerService.geefRondeNummer(getModelObject()))));
		MammaOnderzoek onderzoek = getModelObject().getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek();

		String formattedRondeDatum = Constants.getDateTimeFormat().format(onderzoek.getCreatieDatum());
		Label verslagDatumLabel = new Label("onderzoeksdatum", Model.of(formattedRondeDatum));
		add(verslagDatumLabel);

		BeoordelingsEenheid beoordelingsEenheid = onderzoek.getScreeningsEenheid().getBeoordelingsEenheid();
		Label organisatieLabel = new Label("organisatie", Model.of(beoordelingsEenheid.getParent().getRegio().getNaam()));
		add(organisatieLabel);

		addCheckbox(selectedOnderzoeken);
	}

	protected void addCheckbox(HibernateCheckBoxListContainer<MammaOnderzoek> selectedOnderzoeken)
	{
		MammaOnderzoek onderzoek = getModelObject().getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek();
		CheckBox select = new CheckBox("select", new PropertyModel<>(selectedOnderzoeken.getValueMap(), onderzoek.getId().toString()));
		add(select);
		selectedOnderzoeken.addObject(onderzoek);
	}
}
