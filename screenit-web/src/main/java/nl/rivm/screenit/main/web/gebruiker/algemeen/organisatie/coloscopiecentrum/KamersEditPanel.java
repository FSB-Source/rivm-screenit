package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.coloscopiecentrum;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.ActiefCellPanel;
import nl.rivm.screenit.main.web.component.table.ActiefHeaderInFormPanel;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class KamersEditPanel extends GenericPanel<ColoscopieCentrum>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private HibernateService hibernateService;

	private Component header;

	public KamersEditPanel(String id, IModel<ColoscopieCentrum> centrumModel)
	{
		super(id, centrumModel);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		final BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);

		ColoscopieCentrum centrum = getModelObject();
		final Actie actie = autorisatieService.getActieVoorOrganisatie(ScreenitSession.get().getLoggedInInstellingGebruiker(), centrum, Recht.GEBRUIKER_BEHEER_CC_LOCATIES);

		ScreenitForm<ColoscopieCentrum> form = new ScreenitForm<>("form");
		add(form);

		final WebMarkupContainer refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(true);
		form.add(refreshContainer);
		Kamer searchObject = new Kamer();
		searchObject.setActief(true);
		final IModel<Kamer> searchObjectModel = Model.of(searchObject);
		header = new ActiefHeaderInFormPanel<>("actiefHeader", refreshContainer, searchObjectModel);
		form.add(header);

		ScreenitAjaxLink kamerToevoegen = new ScreenitAjaxLink("kamerToevoegen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ColoscopieCentrum coloscopieCentrum = KamersEditPanel.this.getModelObject();
				Kamer kamer = new Kamer();
				kamer.setName("");
				kamer.setActief(true);
				kamer.setColoscopieCentrum(coloscopieCentrum);
				coloscopieCentrum.getKamers().add(kamer);
				if (Boolean.FALSE.equals(searchObjectModel.getObject().getActief()))
				{
					searchObjectModel.getObject().setActief(Boolean.TRUE);
					ActiefHeaderInFormPanel<Kamer> newHeader = new ActiefHeaderInFormPanel<>("actiefHeader", refreshContainer, searchObjectModel);
					header.replaceWith(newHeader);
					header = newHeader;
					target.add(header);
				}
				target.add(refreshContainer);
			}
		};
		kamerToevoegen.setVisible(actie != null && actie.getNiveau() >= Actie.TOEVOEGEN.getNiveau());
		form.add(kamerToevoegen);

		ListView<Kamer> kamers = new ListView<Kamer>("kamers", new PropertyModel<List<Kamer>>(getModel(), "kamers"))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<Kamer> item)
			{
				Boolean kamerActief = item.getModelObject().getActief();
				boolean visible = false;
				Boolean searchActief = searchObjectModel.getObject().getActief();

				if (Boolean.TRUE.equals(searchActief) && !Boolean.FALSE.equals(kamerActief))
				{
					visible = true;
				}
				else if (Boolean.FALSE.equals(searchActief) && Boolean.FALSE.equals(kamerActief))
				{
					visible = true;
				}
				else if (searchActief == null)
				{
					visible = true;
				}
				item.setVisible(visible);

				boolean magVerwijderen = actie != null && actie.getNiveau() >= Actie.VERWIJDEREN.getNiveau();
				boolean magAanpassen = actie != null && actie.getNiveau() >= Actie.AANPASSEN.getNiveau();

				item.add(new TextField<>("naamEdit", new CompoundPropertyModel<>(new PropertyModel<>(item.getModel(), "name"))).setRequired(true).setLabel(Model.of("Naam kamer"))
					.setEnabled(magAanpassen));

				item.add(new ActiefCellPanel<Kamer>("actiefToggle", item.getModel(), magVerwijderen, dialog, "question.remove.kamer")
				{

					private static final long serialVersionUID = 1L;

					@Override
					protected void onAfterToggleActief(AjaxRequestTarget target, Kamer kamer)
					{
						if (kamer.getId() != null)
						{
							hibernateService.saveOrUpdate(kamer);
						}
						target.add(refreshContainer);
					}

					@Override
					protected boolean isActief(IModel<Kamer> rowModel)
					{
						return !Boolean.FALSE.equals(rowModel.getObject().getActief());
					}
				});
			}
		};
		refreshContainer.add(kamers);
		ScreenitAjaxLink opslaan = new ScreenitAjaxLink("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ColoscopieCentrum coloscopieCentrum = KamersEditPanel.this.getModelObject();
				List<Kamer> activeKamers = new ArrayList<>();
				for (Kamer kamer : coloscopieCentrum.getKamers())
				{
					if (!Boolean.FALSE.equals(kamer.getActief()))
					{
						activeKamers.add(kamer);
					}
				}
				Set<String> kamers = new HashSet<String>();
				for (Kamer kamer : activeKamers)
				{
					String kamerStringToUpper = kamer.getName().toUpperCase();
					kamers.add(kamerStringToUpper);
				}

				if (kamers.size() < activeKamers.size())
				{
					error("Het coloscopie centrum mag geen kamers met dezelfde naam bevatten.");
				}
				else
				{
					onSaveOrUpdateKamers(target, coloscopieCentrum);
				}
			}

		};
		boolean magAanpassen = actie != null && actie.getNiveau() >= Actie.AANPASSEN.getNiveau();
		opslaan.setVisible(magAanpassen);
		form.add(opslaan);
		form.setDefaultButton(opslaan);

	}

	protected abstract void onSaveOrUpdateKamers(AjaxRequestTarget target, ColoscopieCentrum coloscopieCentrum);

}
