package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.huisarts;

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

import java.util.List;

import nl.rivm.screenit.main.service.cervix.CervixHuisartsService;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.PatternValidator;

public abstract class AanvullendeHaLocatieEditPanel extends GenericPanel<CervixHuisartsLocatie>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private CervixHuisartsService cervixUitstrijkendArtsService;

	public AanvullendeHaLocatieEditPanel(String id, IModel<CervixHuisartsLocatie> model, boolean alleenInzien)
	{
		super(id, model);

		Form<CervixHuisartsLocatie> form = new Form<>("form", model);
		add(form);

		ComponentHelper.addTextField(form, "naam", true, 70, String.class, alleenInzien);
		ComponentHelper.addTextField(form, "zorgmailklantnummer", true, 9, String.class, true).add(new PatternValidator("\\d{9,9}"));
		ComponentHelper.addTextField(form, "iban", true, 34, String.class, true);
		ComponentHelper.addTextField(form, "ibanTenaamstelling", true, 70, String.class, true);

		ComponentHelper.addTextField(form, "locatieAdres.straat", true, 43, String.class, alleenInzien);
		ComponentHelper.addTextField(form, "locatieAdres.huisnummer", true, 10, Integer.class, alleenInzien);
		ComponentHelper.addTextField(form, "locatieAdres.huisnummerToevoeging", false, 26, String.class, alleenInzien);
		PostcodeField postcodeField = new PostcodeField("locatieAdres.postcode", String.class);
		postcodeField.setEnabled(!alleenInzien);
		postcodeField.setRequired(true);
		form.add(postcodeField);

		List<Woonplaats> alleWoonplaatsen = hibernateService.loadAll(Woonplaats.class);
		form.add(new ScreenitDropdown<Woonplaats>("locatieAdres.woonplaats", new SimpleListHibernateModel<Woonplaats>(alleWoonplaatsen), new IChoiceRenderer<Woonplaats>()
		{
			@Override
			public Object getDisplayValue(Woonplaats object)
			{
				return (Object) object.getNaam() + " (Gemeente: " + object.getGemeente().getNaam() + ")";
			}

			@Override
			public String getIdValue(Woonplaats object, int index)
			{
				return object.getId().toString();
			}

			@Override
			public Woonplaats getObject(String id, IModel<? extends List<? extends Woonplaats>> choices)
			{
				if (id != null)
				{
					return choices.getObject().stream().filter(w -> w.getId().toString().equals(id)).findFirst().orElse(null);
				}
				return null;
			}
		}).setRequired(true).setEnabled(!alleenInzien));

		add(new AjaxSubmitLink("opslaan", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				CervixHuisartsLocatie locatie = (CervixHuisartsLocatie) form.getModelObject();
				if (!cervixUitstrijkendArtsService.isAndereLocatieMetNaam(locatie))
				{
					cervixUitstrijkendArtsService.saveOrUpdateLocatie(locatie);
					opslaan(target);
				}
				else
				{
					error("Er mag maar een locatie zijn met de naam " + locatie.getNaam() + ". Kies een andere naam voor de locatie.");
				}
			}
		});

	}

	public abstract void opslaan(AjaxRequestTarget target);
}
