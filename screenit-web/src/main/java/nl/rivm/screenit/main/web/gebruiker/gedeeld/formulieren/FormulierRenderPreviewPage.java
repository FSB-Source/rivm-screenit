
package nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren;

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

import nl.dries.wicket.hibernate.dozer.DozerModel;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieVerslagContent;
import nl.rivm.screenit.model.colon.verslag.mdl.MdlVerslagContent;
import nl.rivm.screenit.model.colon.verslag.pa.PaVerslagContent;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.formulieren.TypeFormulier;
import nl.rivm.screenit.model.mamma.verslag.followup.MammaFollowUpVerslagContent;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;

import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.IModel;

public abstract class FormulierRenderPreviewPage extends GebruikerBasePage
{

	private static final long serialVersionUID = 1L;

	public FormulierRenderPreviewPage(IModel<ScreenitFormulierInstantie> formulierInstantie)
	{
		add(new ScreenitFormulierRenderPanel("panel", formulierInstantie)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected IModel<FormulierResultaatImpl> createRenderContextModel(ScreenitFormulierInstantie formulierInstantie, FormulierRenderContext formulierRenderContext)
			{
				TypeFormulier typeFormulier = formulierInstantie.getTypeFormulier();
				switch (typeFormulier)
				{
				case MDL:
					formulierRenderContext.setRootObject(new DozerModel<>(new MdlVerslagContent()));
					break;
				case PALGA:
					formulierRenderContext.setRootObject(new DozerModel<>(new PaVerslagContent()));
					break;
				case CYTOLOGIE:
					formulierRenderContext.setRootObject(new DozerModel<>(new CervixCytologieVerslagContent()));
					break;
				case MAMMA_PA_FOLLOW_UP:
					formulierRenderContext.setRootObject(new DozerModel<>(new MammaFollowUpVerslagContent()));
					break;
				default:
					break;
				}
				return super.createRenderContextModel(formulierInstantie, formulierRenderContext);
			}
		});
		add(new Link<Void>("terug")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				terug();
			}

		});
	}

	protected abstract void terug();
}
