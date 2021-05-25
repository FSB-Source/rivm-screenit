package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.standplaats;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.UploadDocumentDownloadLinkPanel;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;

public class MammaStandplaatsViewLocatieAdresPanel extends GenericPanel<MammaStandplaatsLocatie>
{
	private IModel<MammaStandplaats> standplaatModel;

	public MammaStandplaatsViewLocatieAdresPanel(String id, IModel<MammaStandplaatsLocatie> model, IModel<MammaStandplaats> standplaatModel, BootstrapDialog dialog)
	{
		super(id, model);
		this.standplaatModel = standplaatModel;
		setOutputMarkupId(true);
		boolean tijdelijkeLocatie = Boolean.TRUE.equals(model.getObject().getTijdelijk());
		String titel = getString("titel.locatie");
		if (tijdelijkeLocatie)
		{
			titel = getString("titel.tijdelijke.locatie");
		}

		add(new Label("titel", titel));
		boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN)
			&& ScreenitSession.get().getScreeningOrganisatie() != null;

		MammaStandplaatsLocatie locatie = model.getObject();
		boolean isLocatieGevuld = StringUtils.isNotBlank(locatie.getPlaats()) || StringUtils.isNotBlank(locatie.getPostcode());
		boolean toonTM = tijdelijkeLocatie && (locatie.getStartDatum() != null || locatie.getEindDatum() != null);

		add(new Label("straat"));
		add(new Label("huisnummer"));
		add(new Label("huisnummerToevoeging"));
		add(new Label("postcode"));
		add(new Label("plaats").setVisible(isLocatieGevuld));
		add(new Label("startDatum").setVisible(toonTM));
		add(new Label("eindDatum").setVisible(toonTM));

		add(new WebMarkupContainer("geenGegevensIngevuld").setVisible(!isLocatieGevuld));
		ComponentHelper.addTextArea(this, "locatieBeschrijving", false, 255, true);

		add(new Label("standplaatsLocatieBijlage.naam"));
		add(new UploadDocumentDownloadLinkPanel("download", new PropertyModel<UploadDocument>(getModel(), "standplaatsLocatieBijlage")));

		add(ComponentHelper.newCheckBox("brievenApartPrinten", false));
		add(new IndicatingAjaxLink<Void>("wijzigen")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target,
					new MammaStandplaatsEditLocatieAdresPanel(IDialog.CONTENT_ID, MammaStandplaatsViewLocatieAdresPanel.this.getModel(), standplaatModel, dialog)
					{

						@Override
						protected void onClickOpslaan(AjaxRequestTarget target)
						{
							Component nieuwVieuwPanel = new MammaStandplaatsViewLocatieAdresPanel(id, MammaStandplaatsViewLocatieAdresPanel.this.getModel(), standplaatModel,
								dialog);
							MammaStandplaatsViewLocatieAdresPanel.this.replaceWith(nieuwVieuwPanel);
							target.add(nieuwVieuwPanel);
						}

					});
			}
		}.setVisible(magAanpassen));
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();

		ModelUtil.nullSafeDetach(standplaatModel);
	}
}
