package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis;

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
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaTehuisService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.table.ActiefCellPanel;
import nl.rivm.screenit.main.web.component.table.ActiefHeaderPanel;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.model.mamma.MammaTehuisOpmerking;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxEventBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.beans.support.PropertyComparator;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class MammaTehuisOpmerkingenPanel extends GenericPanel<MammaTehuis>
{
	@SpringBean
	private MammaTehuisService tehuisService;

	private WebMarkupContainer editOpmerkingContainer;

	private WebMarkupContainer opmerkingen;

	private boolean ingelogdNamensRegio;

	private boolean magAanpassen;

	public MammaTehuisOpmerkingenPanel(String id, IModel<MammaTehuis> model)
	{
		super(id, model);

		magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_TEHUIS, Actie.AANPASSEN);
		ingelogdNamensRegio = ScreenitSession.get().getScreeningOrganisatie() != null;

		opmerkingen = new WebMarkupContainer("opmerkingen");
		opmerkingen.setOutputMarkupId(true);

		MammaTehuisOpmerking searchObject = new MammaTehuisOpmerking();
		searchObject.setActief(true);
		final IModel<MammaTehuisOpmerking> searchObjectModel = Model.of(searchObject);
		opmerkingen.add(new ActiefHeaderPanel<>("actiefHeader", opmerkingen, searchObjectModel));

		IModel<List<MammaTehuisOpmerking>> listModel = new IModel<List<MammaTehuisOpmerking>>()
		{

			private static final long serialVersionUID = 1L;

			@Override
			public List<MammaTehuisOpmerking> getObject()
			{
				List<MammaTehuisOpmerking> list = new ArrayList<MammaTehuisOpmerking>(getModelObject().getOpmerkingen());
				Collections.sort(list, new PropertyComparator<>("creatieDatum", false, false));
				return list;
			}
		};
		ListView<MammaTehuisOpmerking> list = new ListView<MammaTehuisOpmerking>("list", listModel)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(ListItem<MammaTehuisOpmerking> item)
			{
				MammaTehuisOpmerking opmerking = item.getModelObject();
				Boolean searchActief = searchObjectModel.getObject().getActief();
				Boolean opmerkingActief = opmerking.getActief();
				boolean visible = false;
				if (Boolean.TRUE.equals(searchActief) && !Boolean.FALSE.equals(opmerkingActief))
				{
					visible = true;
				}
				else if (Boolean.FALSE.equals(searchActief) && Boolean.FALSE.equals(opmerkingActief))
				{
					visible = true;
				}
				else if (searchActief == null)
				{
					visible = true;
				}
				item.setVisible(visible);

				if (ingelogdNamensRegio && magAanpassen)
				{
					item.add(new AjaxEventBehavior("click")
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected void onEvent(AjaxRequestTarget target)
						{
							editOpmerking(target, ModelUtil.cModel(item.getModelObject()));
						}
					});
				}
				item.add(DateLabel.forDatePattern("creatieDatum", Model.of(opmerking.getCreatieDatum()), "dd-MM-yyyy HH:mm"));
				item.add(new Label("opmerking", opmerking.getOpmerking()));

				item.add(
					new ActiefCellPanel<MammaTehuisOpmerking>("actiefToggle", item.getModel(), ingelogdNamensRegio && magAanpassen, null, null)
					{

						private static final long serialVersionUID = 1L;

						@Override
						protected boolean isActief(IModel<MammaTehuisOpmerking> rowModel)
						{
							return !Boolean.FALSE.equals(rowModel.getObject().getActief());
						}

						@Override
						protected void onAfterToggleActief(AjaxRequestTarget target, MammaTehuisOpmerking actiefObject)
						{
							super.onAfterToggleActief(target, actiefObject);
							target.add(opmerkingen);
							tehuisService.saveOrUpdateTehuisOpmerking(actiefObject, null, ScreenitSession.get().getLoggedInInstellingGebruiker());
						}

						@Override
						protected boolean skipConfirmation()
						{
							return true;
						}
					});
			}

		};
		opmerkingen.add(list);
		add(opmerkingen);

		IndicatingAjaxLink<Void> nieuweOpmerking = new IndicatingAjaxLink<Void>("nieuw")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				IModel<MammaTehuisOpmerking> nieuweOpmerking = ModelUtil.cRModel(new MammaTehuisOpmerking());
				nieuweOpmerking.getObject().setActief(true);
				editOpmerking(target, nieuweOpmerking);
			}
		};
		nieuweOpmerking.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.TOEVOEGEN) && ingelogdNamensRegio);
		add(nieuweOpmerking);

		editOpmerkingContainer = new WebMarkupContainer("editOpmerkingContainer");
		editOpmerkingContainer.setOutputMarkupPlaceholderTag(true);
		editOpmerkingContainer.setVisible(false);
		add(editOpmerkingContainer);
	}

	private void editOpmerking(AjaxRequestTarget target, IModel<MammaTehuisOpmerking> opmerking)
	{
		WebMarkupContainer opmerkingContainer = new WebMarkupContainer("editOpmerkingContainer");
		opmerkingContainer.setOutputMarkupId(true);
		editOpmerkingContainer.replaceWith(opmerkingContainer);
		editOpmerkingContainer = opmerkingContainer;
		target.add(opmerkingContainer);

		Form<?> opmerkingForm = new Form<>("opmerkingForm", opmerking);
		opmerkingContainer.add(opmerkingForm);

		ComponentHelper.addTextArea(opmerkingForm, "opmerking", true, 4096, false);

		opmerkingForm.add(new IndicatingAjaxButton("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaTehuis Tehuis = (MammaTehuis) MammaTehuisOpmerkingenPanel.this.getDefaultModelObject();
				MammaTehuisOpmerking opmerking = (MammaTehuisOpmerking) opmerkingForm.getModelObject();
				boolean changed = tehuisService.saveOrUpdateTehuisOpmerking(opmerking, Tehuis, ScreenitSession.get().getLoggedInInstellingGebruiker());
				target.add(opmerkingen);
				WebMarkupContainer invisibleopmerkingContainer = new WebMarkupContainer("editOpmerkingContainer");
				invisibleopmerkingContainer.setOutputMarkupPlaceholderTag(true);
				invisibleopmerkingContainer.setVisible(false);
				editOpmerkingContainer.replaceWith(invisibleopmerkingContainer);
				editOpmerkingContainer = invisibleopmerkingContainer;
				target.add(invisibleopmerkingContainer);
				if (changed)
				{
					success(getString("message.gegevensopgeslagen"));
				}
				BasePage.markeerFormulierenOpgeslagen(target);
			}
		});
	}
}
