package nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole.reeks;

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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.SKMLInterneControleSet;
import nl.rivm.screenit.model.colon.SKMLSentineelControleBarcode;
import nl.rivm.screenit.model.colon.enums.SKMLSentineelControleType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.KwaliteitscontroleService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_BEHEER_SKML_INTERNE_CONTROLE,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class SKMLInterneControleConfigPage extends KwaliteitscontroleBasePage
{

	private static final long serialVersionUID = 1L;

	private IModel<List<SKMLInterneControleSet>> allSkmlInterneModels;

	@SpringBean
	private KwaliteitscontroleService kwaliteitscontroleService;

	private static final int MAXLENGTH = 255;

	public SKMLInterneControleConfigPage()
	{
		add(new SentineelcontrolesForm("form"));
	}

	private class SentineelcontrolesForm extends ScreenitForm<SKMLSentineelControleBarcode>
	{

		private static final long serialVersionUID = 1L;

		public SentineelcontrolesForm(String id)
		{
			super(id);

			List<SKMLInterneControleSet> allInterneControleSets = kwaliteitscontroleService.createOrGetAllInterneControleSets();
			allSkmlInterneModels = ModelUtil.listModel(allInterneControleSets);

			ListView<SKMLInterneControleSet> sets = new ListView<SKMLInterneControleSet>("set", allSkmlInterneModels)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void populateItem(ListItem<SKMLInterneControleSet> item)
				{
					item.add(new Label("volgorde", item.getModelObject().getVolgorde()));
					item.add(new TextField<String>("controleTekst", new PropertyModel<String>(item.getModel(), "controleTekst")).add(StringValidator.maximumLength(MAXLENGTH)));
					item.add(new TextField<String>("qbaseId", new PropertyModel<String>(item.getModel(), "qbaseId")));
				}
			};
			add(sets);
			add(new IndicatingAjaxButton("opslaan")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					try
					{
						kwaliteitscontroleService.saveOrUpdateInterneIdSets(allSkmlInterneModels.getObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());
						info("Set's zijn opgeslagen.");
					}
					catch (IllegalArgumentException e)
					{
						error("Of 'Controle tekst' EN 'QBase ID' of geen van beiden moeten gevuld zijn. Niets opgeslagen.");
					}
				}
			});
		}

		private TextField<SKMLSentineelControleBarcode> addTextField(String id, SKMLSentineelControleType typeCup)
		{
			IModel<SKMLSentineelControleBarcode> model = new PropertyModel<SKMLSentineelControleBarcode>(allSkmlInterneModels, "[" + typeCup.ordinal() + "].barcode");
			return new TextField<>(id, model);
		}

		@Override
		protected void onDetach()
		{
			super.onDetach();
			ModelUtil.nullSafeDetach(allSkmlInterneModels);
		}
	}
}
